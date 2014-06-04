;;; This is essentially the lisp version library/tcrypt.js (from turtl/js).

(in-package :turtl-core)

(define-condition crypto-error (turtl-error) ()
  (:documentation "General crypto error."))

(define-condition crypto-authentication-failure (crypto-error) ()
  (:documentation "Authentication of HMAC failed."))

(define-constant +crypto-version+ 6
  :documentation "Our (de)serialization version."
  :test '=)


;; -----------------------------------------------------------------------------
;; Payload description options (stored in plaintext with each encrypted item).
;;
;; NOTE: never inject items into these lists...only append them!!!!
;; NOTE: these lists can only support 256 items each!!!!
;; -----------------------------------------------------------------------------
(define-constant +cipher-index+
  #(:aes)
  :test 'equalp)
(define-constant +block-index+
  #(:cbc :gcm)
  :test 'equalp)
(define-constant +padding-index+
  #(:ansix923 :pkcs7)
  :test 'equalp)
(define-constant +kdf-index+
  #(#(:sha256 2 64))
  :test 'equalp)

;; these are defaults, and the values are indxes in the above arrays
(define-constant +default-cipher+ 0)
(define-constant +default-block+ 1)
(define-constant +default-padding+ 1)
(define-constant +default-kdf-mode+ 0)
;; -----------------------------------------------------------------------------


(defun decode-payload-description (version desc)
  "Given a serialization version and a payload description, pull out any
   pertinant information (cipher, block mode, padding, etc)."
  (let ((res nil))
    (when (<= 1 version)
      (setf (getf res :cipher) (aref +cipher-index+ (aref desc 0)))
      (setf (getf res :block-mode) (aref +block-index+ (aref desc 1)))
      (when (<= version 4)
        (setf (getf res :kdf-mode) (aref desc 3))
        (setf (getf res :padding-index) (aref desc 2))))
    res))

(defun encode-payload-description (version &key cipher block-mode padding kdf-mode)
  "Given a serialization version and a set of information about how a payload is
   serialized, return a payload description."
  (unless (and cipher block-mode)
    (error 'crypto-error :msg "encode-payload-description: must provide both cipher and block-mode"))
  (let ((desc (make-array 0 :element-type 'nec:octet :adjustable t :fill-pointer t)))
    (when (<= 1 version)
      (let ((cipher-idx (position cipher +cipher-index+))
            (block-idx (position block-mode +block-index+)))
        (vector-push-extend cipher-idx desc)
        (vector-push-extend block-idx desc)
        (when (<= version 4)
          (let ((padding-idx (position padding +padding-index+)))
            (vector-push-extend padding-idx desc)
            (vector-push-extend kdf-mode desc)))))
    desc))

(defun authenticate-payload (passphrase version description iv ciphertext)
  "Authenticate a crypto payload via HMAC. Basically, HMAC everything in the
   payload BESIDES the included HMAC.
   
   NOTE: this is now vestigial as all crypto version 5 and higher uses block
   modes with built-in encryption (GCM).
   
   NOTE: if version <= 4, we mimic how the JS version of this function worked,
   which is a buggy concatenation:
     3 + 2 + 'asdf'
   which was thought to yield '32asdf' however yielded '5asdf'. great. although
   this function is only used for old data currently, it may be used again in
   the future for authenticated crypto modes, so we implement the non-buggy
   behavior as well as the buggy for backwards compat."
  (let* ((payload (concatenate 'nec:octet-array
                               (if (<= version 4)
                                   ;; implement buggy, JS-compatible version
                                   ;; 3 + 2 + "desc" => "5desc"
                                   (babel:string-to-octets (write-to-string (+ version (length description))))
                                   ;; *correct* version of concatenation
                                   ;; 3 + 2 + "desc" => #(0 3 2 "d" "e" "s" "c")
                                   (vector (logand (ash version -8) #xff)
                                           (logand version #xff)
                                           (length description)))
                               description
                               iv
                               ciphertext)))
    (format t "payload (~a): ~s~%" (length payload) payload)
    (format t "hmac key: ~s~%" (to-base64 passphrase))
    (nec:hmac-sha256 passphrase payload)))

(defun derive-keys (master-key &key (hasher :sha1) (iterations 50) (key-size 64))
  "Given a master key and a set of params, derive two subkeys: one for
   encryption/decryption and one for HMAC generation.
   
   NOTE: this is now vestigial as newer serialization versions (>= 5) use GCM
   for authentication."
  (let* ((both-keys (keygen master-key nil iterations key-size :hasher hasher))
         (enc-key (subseq both-keys 0 32))
         (hmac-key (subseq both-keys 32)))
    (list :crypto enc-key :hmac hmac-key)))

(defun deserialize (enc &key hmac-only raw)
  "Deserialize the standard serialization format, as follows:
   
	   |-2 bytes-| |-1 byte----| |-N bytes-----------| |-16 bytes-| |-N bytes----|
	   | version | |desc length| |payload description| |    IV    | |payload data|

	 - version tells us the serialization version. although it will probably not
     get over 255, it has two bytes just in case. never say never.
	 - desc length is the length of the payload description, which may change in
     length from version to version.
	 - payload description tells us what algorithm/format the encryption uses.
     for instance, it could be AES+CBC, or Twofish+CBC, etc etc. payload
     description encoding/length may change from version to version.
	 - IV is the initial vector of the payload, in binary form
	 - payload data is our actual data, encrypted.
   
   Note that older versions of the serialization format (1 <= version <= 4) also
   contain an HMAC authentication hash directly after the version (removed
   because GCM)."
  (let ((idx 0)
        (version nil)
        (hmac nil)
        (desc nil)
        (iv nil)
        (ciphertext nil))
    (setf version (+ (ash (aref enc idx) 8)
                     (aref enc (1+ idx))))
    (incf idx 2)

    ;; check if we have serialization version 0 (by testing for a version higher
    ;; than normal).
    ;;
		;; TODO: if we ever get above 1000 versions, change this. The lowest
		;; allowable Base64 message is '++', which translates to 11,051 but for now
		;; we'll play it safe and cap at 1K
    (when (< 1000 version)
      (let* ((split (position (char-code #\:) enc))
             (ciphertext (from-base64 (babel:octets-to-string (subseq enc 0 split))))
             (iv (from-hex (subseq enc (+ 2 split))))
             (res (list :version 0
                        :ciphertext ciphertext
                        :cipher :aes
                        :block-mode :cbc
                        :padding :ansix923
                        :iv iv)))
        (return-from deserialize res)))

    ;; only deal with HMACs when ver <= 4
    (when (<= version 4)
      (setf hmac (subseq enc idx (+ idx 32)))
      (incf idx 32)
      (when hmac-only
        (return-from deserialize hmac)))
    
    ;; grab/decode the description
    (let ((desc-length (aref enc idx)))
      (incf idx)
      (setf desc (subseq enc idx (+ idx desc-length)))
      (incf idx desc-length))
    
    ;; grab the iv
    (setf iv (subseq enc idx (+ idx 16)))
    (incf idx 16)

    (when raw
      ;; return no ciphertext, just everything before
      (return-from deserialize (subseq enc 0 idx)))

    (setf ciphertext (make-array (- (length enc) idx) :displaced-to enc :displaced-index-offset idx))
    
    (let ((res (list :version version
                     :desc desc
                     :iv iv
                     :ciphertext ciphertext)))
      (when hmac
        (setf (getf res :hmac) hmac))
      res)))

(defun serialize (enc version &key desc iv hmac)
  "Serialize our encryption data into the standard format (see deserialize for
   details)."
  (let ((serialized (flexi-streams:make-in-memory-output-stream :element-type 'nec:octet))
        (version (logand version #xffff)))
    ;; handle version 0 stupidity
    (when (zerop version)
      (let* ((crypto (concatenate 'string (to-base64 enc) ":i" (to-hex iv))))
        (return-from serialize crypto)))
    (write-byte (ash version -8) serialized)
    (write-byte (logand version #xff) serialized)
    (when (<= version 4)
      (write-sequence hmac serialized))
    (write-byte (logand (length desc) #xff) serialized)
    (write-sequence desc serialized)
    (write-sequence iv serialized)
    (concatenate 'nec:octet-array
                 (flexi-streams:get-output-stream-sequence serialized)
                 enc)))

(defun encrypt (key plaintext
                    &key (version +crypto-version+) iv
                         (cipher (aref +cipher-index+ +default-cipher+))
                         (block-mode (aref +block-index+ +default-block+)))
  "Encrypt the given plaintext, and serialize the result in our standard format."
  ;; TODO: implement switchable ciphers/block modes
  (setf cipher :aes)
  (setf block-mode :gcm)

  (assert (find cipher +cipher-index+))
  (assert (find block-mode +block-index+))
  (let ((version (if (zerop version)
                     version
                     ;; force lastest version (unless 0) since only decryption
                     ;; needs to support older versions.
                     +crypto-version+))
        (plaintext (if (stringp plaintext)
                       (babel:string-to-octets plaintext :encoding :utf-8)
                       plaintext))
        (iv (or iv (make-iv))))

    ;; handle version 0 stupidity (needed for user auth tokens)
    (when (zerop version)
      (let* ((ciphertext (nec:encrypt-aes-cbc key
                                              plaintext
                                              iv
                                              :padding 'nec:pad-ansix923))
             (serialized (serialize ciphertext 0 :iv iv)))
        (return-from encrypt serialized)))

    ;; TODO: actually implement cipher/block-mode here. right now we just
    ;; encrypt using the latest method available (GCM)
    (let* ((desc (encode-payload-description version :cipher cipher :block-mode block-mode))
           ;; note we pass ciphertext as #() here since we are going to use this
           ;; serialized data for authentication used to generate the ciphertext
           (auth (serialize #() version :desc desc :iv iv))
           (ciphertext (nec:encrypt-aes-gcm key plaintext iv auth)))
      (concatenate 'nec:octet-array
                   auth
                   ciphertext))))

(defun fix-utf-key (utf8-key)
  "Wow. Such fail. In older versions of Turtl, keys were UTF8 encoded strings.
   This function converts the keys back."
  (let ((key-str (babel:octets-to-string utf8-key :encoding :utf-8))
        (bytes (make-array 32 :element-type 'nec:octet)))
    (loop for i from 0
          for c across key-str do
      (setf (aref bytes i) (char-code c)))
    bytes))

(defun decrypt (key ciphertext)
  "Given a key and a ciphertext (both byte vectors), decrypt the ciphertext and
   return the result (a byte vector).
   
   All the information needed to decrypt the ciphertext (cipher, block mode, iv,
   padding, key derivation method, etc) is stored in the ciphertext itself."
  ;; due to some errors in judgement, early keys were encoded as utf8 strings.
  ;; luckily we can detect them and fix them when decrypting.
  (when (< 32 (length key))
    (setf key (fix-utf8-key key)))
  (let* ((enc (deserialize ciphertext))
         (version (getf enc :version))
         (desc (if (zerop version)
                   (list :cipher :aes
                         :block-mode :cbc)
                   (decode-payload-description version (getf enc :desc))))
         (iv (getf enc :iv))
         (payload (getf enc :ciphertext))
         (decrypted nil))
    (cond ((zerop version)
           (setf decrypted (nec:decrypt-aes-cbc key payload iv)))
          ((<= version 4)
           (let* ((kdf-entry (aref +kdf-index+ (getf desc :kdf-mode)))
                  (keys (derive-keys key
                                     :hasher (aref kdf-entry 0)
                                     :iterations (aref kdf-entry 1)
                                     :key-size (aref kdf-entry 2)))
                  (crypto-key (getf keys :crypto))
                  (hmac-key (getf keys :hmac))
                  (payload-hmac (getf enc :hmac))
                  (compare-hmac (authenticate-payload hmac-key
                                                      version
                                                      (getf enc :desc)
                                                      iv
                                                      payload)))
             (unless (nec:secure-equal payload-hmac compare-hmac)
               (error 'crypto-authentication-failure :msg "Payload HMACs did not match (either you have tampered data or a bad key)"))
             (setf decrypted (nec:decrypt-aes-cbc crypto-key payload iv))))
          ((<= 5 version)
           (let ((auth (deserialize ciphertext :raw t)))
             (handler-case
               (setf decrypted (nec:decrypt-aes-gcm key payload iv auth))
               (nec:nettle-auth-error ()
                 (error 'crypto-authentication-failure :msg "GCM authentication failed."))))))

    ;; made it. if were version 4 or 5, remove the leading utf8 byte. this is an
    ;; obnoxious vestigial requirement from the JS/tcrypt days.
    (when (<= 4 version 5)
      (setf decrypted (subseq decrypted 1)))
    decrypted))

(defun ctest()
(let* ((key (key-from-string "wqBkfcKUwqbCpcKiHFsqLsOzL8O3wqAEfMKpc8OsCWMtQC4iecOHwobDg1fDtw=="))
       (ciphertext (from-base64 "AAPaSAJDTCyvIFeveVQEiaSPhVmLauA8AugseiHwtya1uwQAAAAAnABM9+1DPTYxbpiNnA3oJIOfJi2TGkduA9lDKLjT9eq3k5cHnka+l1HXP9b295WS25AxO3rn5/jbnUUOKXCPmLvJnvcyc3ba42WnjF8NsyMws8QnIli78MxnrCvZWWG1tNgpbg4TlfnoBKRgp+d5MLpuCe1LqHYHdPU2NjaLwF8WVhK0HnHBuM6/CMzgBW/nOn1Cb5O4yfhUoGzUiVDa1AooZ+0iL8vBIkJXI7ATXnhLSUMXnyQUJcD5vcbKGMaC8Sb5/OjR80GFEMjTDREPxx1rZcWoUYfudv5mVEo1HZYlmqt4qeNb0sVVnz/bzzunn/o7fp2tvVkK1jR/MGeHNPWYeTPeTHgD7EH5hJAxygnaIzUDpCYraEYvycnpORwF694LuykW+CFTa4tlTReE+6Z0zlC2K639lAn3uDm0QuS85Jmd8XvZyh1w3GOs1S94IlgUnbDX4zrzxvC6AEFaUcuCA6CrP5rksMM4FdEvQSE7Cz6RPNQR7CEmxmUKH7NxPPAYPCkuVHfXSPk/dq7aq/j2vfcfHNvSVTmQ0I7QWQk6PFmA1rnR46MhlPBTaViO"))
       (plain (decrypt key ciphertext)))
  (format t "dec: ~s~%" (babel:octets-to-string plain)))
)

(defun keygen (passphrase salt iterations key-size &key (hasher :sha1))
  "Generate a key from a passphrase using PBKDF2."
  (nec:pbkdf2 passphrase (or salt #()) iterations key-size :hasher hasher))

(defun to-base64 (bytes)
  "Convert the given bytes to base64."
  (base64:usb8-array-to-base64-string bytes))

(defun from-base64 (string)
  "Convert a base64 string to bytes."
  (base64:base64-string-to-usb8-array string))

(defun to-hex (bytes)
  "Convert a byte array to a string of hex characters."
  (string-downcase
    (with-output-to-string (s)
      (loop for x across bytes do
        (format s "~2,'0x" x)))))

(defun from-hex (string)
  "Convert a hex string to a byte array."
  (assert (evenp (length string)))
  (when (typep string 'nec:octet-array)
    (setf string (babel:octets-to-string string)))
  (let ((bytes (make-array (/ (length string) 2) :element-type 'nec:octet)))
    (dotimes (i (/ (length string) 2))
      (setf (aref bytes i) (parse-integer string :radix 16 :start (* i 2) :end (+ (* i 2) 2))))
    bytes))

(defun key-to-string (key)
  "Convert a key to string form."
  (to-base64 key))

(defun key-from-string (key-str)
  "Convert a string key representation to key bytes."
  (from-base64 key-str))

(defun random-bytes (num-bytes)
  "Gets N random bytes."
  (nec:random-bytes num-bytes))

(defun make-iv (&optional value)
  "Make a new IV from the given seed (if seed is 0 length or less than 16 bytes
   then random data is appended)."
  (let ((length (length value)))
    (when (zerop (length value))
      (return-from make-iv (nec:random-bytes 16)))
    (when (< length 16)
      (setf value (concatenate 'nec:octet-array value (nec:random-bytes 16))))
    (when (< 16 (length value))
      (setf value (subseq value 0 16)))
    value))

(defun random-key ()
  "Generate a random encryption key."
  (random-bytes 32))

(defun thash (data &key (hasher 'sha256))
  "Hash the given data (defaults to sha256)."
  (let ((hash-fn (intern (string hasher) 'nec)))
    (assert (fboundp hash-fn))
    (funcall hash-fn data)))

(defun random-number ()
  "Returns a random float between 0 and 1."
  (nec:random-float))

(defun uuid ()
  (string-downcase
    (cl-ppcre:regex-replace-all
      "[xy]"
      "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx"
      (lambda (match &rest args)
        (let ((c (subseq match (caddr args) (cadddr args))))
          (let* ((r (floor (* (random-number) 16)))
                 (v (if (eq (aref c 0) #\x)
                        r
                        (logior (logand r #x3) #x8))))
            (write-to-string v :base 16)))))))

