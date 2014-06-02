;;; This is essentially the lisp version library/tcrypt.js (from turtl/js).

(in-package :turtl-core)

(define-condition crypto-error (turtl-error) ()
  (:documentation "General crypto error."))

(define-constant +crypto-version+ 5
  :documentation "Our (de)serialization version."
  :test '=)

;; -----------------------------------------------------------------------------
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
;; -----------------------------------------------------------------------------

;; these are defaults, and the values are indxes in the above arrays
(define-constant +default-cipher+ 0)
(define-constant +default-block+ 1)
(define-constant +default-padding+ 1)
(define-constant +default-kdf-mode+ 0)

(defun decode-payload-description (version desc)
  "Given a serialization version and a payload description, pull out any
   pertinant information (cipher, block mode, padding, etc)."
  (let ((res nil))
    (when (<= 1 version)
      (setf (getf res :cipher) (aref +cipher-index+ (aref desc 0)))
      (setf (getf res :block-mode) (aref +block-index+ (aref desc 1)))
      (when (<= 4 version)
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
  "Authenticate a crypto payload via HMAC.
   
   NOTE: this is now vestigial as all crypto version 5 and higher uses block
   modes with built-in encryption (GCM)."
  (let* ((payload (concatenate 'nec:octet-array
                               (babel:string-to-octets (write-to-string version))
                               (length description)
                               description
                               iv
                               ciphertext)))
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
             (ciphertext (from-base64 (subseq enc 0 split)))
             (iv (from-hex (subseq enc (1+ split))))
             (res (list :ciphertext ciphertext
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

    (let* ((desc (encode-payload-description version :cipher cipher :block-mode block-mode))
           ;; note we pass ciphertext as #() here since we are going to use this
           ;; serialized data for authentication used to generate the ciphertext
           (auth (serialize #() version :desc desc :iv iv))
           (ciphertext (nec:encrypt-aes-gcm key
                                            plaintext
                                            iv
                                            auth)))
      (concatenate 'nec:octet-array
                   auth
                   ciphertext))))

(identity
(encrypt (key-from-string "oPCkqo99Egq1kvkYAGcL0LnovZORZ2SBpcT49Flf9yI=")
         (babel:string-to-octets "IGNORE ME!!!")
         :iv (from-base64 "Cek8BUuta935WkoOWJKQIA=="))
)

(defun decrypt (key ciphertext &key (version +crypto-version+) iv)
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

