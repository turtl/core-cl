;;; Note that the majority of these tests assume that SJCL[1] is correct. This
;;; in itself is not entirely bad, because early testing showed that there are
;;; no discrepancies between the data output by SJCL and the data output by
;;; Nettle over a wide variety of circumstances. This means that they are either
;;; both right, or both wrong. The probability of two libraries being wrong in
;;; the exact same ways is small enough that using SJCL as a verification for
;;; the following tests seems a correct solution.
;;;
;;; [1]: http://bitwiseshiftleft.github.io/sjcl/
(in-package :turtl-core-test)

(in-suite turtl-core-crypto)

(test key-tests
  "Test (de)serialization, random creation of keys."
  ;; test key deserialization from base64 (values grabbed from tcrypt)
  (let ((key-str "csUNaLbDnnZitPgkqOvIu8k1EPru+liFBnoOclHfJnw=")
        (key-bytes #(114 197 13 104 182 195 158 118 98 180 248 36 168 235 200 187 201 53 16 250 238 250 88 133 6 122 14 114 81 223 38 124)))
    (is (equalp key-bytes (key-from-string key-str))))
  ;; test key serialization to base64
  (let ((key-bytes #(253 124 166 27 10 241 223 35 68 14 61 210 126 245 17 7 196 93 203 227 67 156 120 83 224 83 247 111 22 88 227 98))
        (key-str "/XymGwrx3yNEDj3SfvURB8Rdy+NDnHhT4FP3bxZY42I="))
    (is (equalp key-str (key-to-string key-bytes))))
  (is (= 32 (length (random-key))))
  (is (typep (random-key) '(simple-array (unsigned-byte 8) (*)))))

(test keygen
  "Test pbkdf2 stuff."
  (let* ((tcrypt-passphrase (babel:string-to-octets "quaint fox mimicks younger pony"))
         (tcrypt-salt (babel:string-to-octets "SALLLLT"))
         (iterations 4000)
         (key-size 32))
    ;; test sha1
    (let ((tcrypt-result (from-base64 "d/j2q3RvunSsE9R7VQQriIqURAZBnfKA9Z19TICGBEI="))
          (turtl-result (keygen tcrypt-passphrase tcrypt-salt iterations key-size)))
      (is (equalp tcrypt-result turtl-result) "sha1 pbkdf2 result mismatch"))
    ;; test sha256
    (let ((tcrypt-result (from-base64 "Ok+KC3S7KLHl0ZtyINe3pbdDoPTJqemfckLPEbMGvX8="))
          (turtl-result (keygen tcrypt-passphrase tcrypt-salt iterations key-size :hasher :sha256)))
      (is (equalp tcrypt-result turtl-result) "sha256 PBKDF2 result mismatch"))))

(test (encryption-test :depends-on key-tests)
  "Tests encrypting version 0 and correllating the results with the Turtl JS
   (tcrypt) output."
  (let* ((plaintext (babel:string-to-octets "i am going to encrypt this, hope it matches with tcrypt."))
         (key (key-from-string "oPCkqo99Egq1kvkYAGcL0LnovZORZ2SBpcT49Flf9yI="))
         (iv (from-base64 "Cek8BUuta935WkoOWJKQIA=="))
         (tcrypt-version "uybWwwL6GivErehe/Pt4JziZM7KBifVvLuJJRIVxIfY2vDoQz+/RrOtRU6F3MM3qiMWOD+KfnhARekzGYG7SZw==:i09e93c054bad6bddf95a4a0e58929020")
         (turtl-version (encrypt key plaintext :version 0 :iv iv)))
    (is (string= tcrypt-version turtl-version))))

(test (decryption-test-version0 :depends-on key-tests)
  "Test decryption of version 0 against Turtl's tcrypt library."
  (let* ((key (key-from-string "WPEpNTwrRE144Y7uLuTmJSIYhc1qoo7OMLvZ3oNwaII="))
         (tcrypt-ciphertext (babel:string-to-octets "VFQYqIsxYBVdVxANyHepjXvowy107j+n9t1bQqcSI2E2CGLscFMnuZJW6vxLz75XuBHKn0lhbC5FFL1HDuXa1bjvj9CSQcuOl96DqQzGs6BuBHBtTZDHovuzlaC+J7eanoAydRmCuz5iZgKNLLWgWox9e3HWcwRrbyGwOAq5Cj/7s0cn4lKDE5K9V/+x3EA4LzB6aOekBcJNPKD9LmV7I2yifELN2+OAJC7jcICYZHa6i0KciLBZUxTeYfpM1vZJ4suWLH5ZdTFdT9SUINbi06WGFyJtTOQrqlzIz2LFHctsm/FDuU8r9bwFc4sYbha/Ej80+z3S7Zjfp40Ra5GW71oLyK6NyuZSjbdK/xShybiqzyEhA6hf6ekH4Mfef0SlGYTKTvCx7bNd+pPJa/R+LkT/qGgDDJkyzqejvP7guhk=:ib043a089740f1d5ed086225eb30063ff"))
         (tcrypt-plaintext "{\"sort\":15,\"type\":\"link\",\"tags\":[\"programming\",\"css\",\"design\"],\"url\":\"http://nicolasgallagher.com/css-drop-shadows-without-images/\",\"title\":\"CSS drop-shadows without images\",\"text\":\"Perhaps a bit dated, but seems to work well. Check [the demo](http://nicolasgallagher.com/css-drop-shadows-without-images/demo/).\"}")
         (turtl-plaintext (babel:octets-to-string (decrypt key tcrypt-ciphertext))))
    (is (string= tcrypt-plaintext turtl-plaintext))))

;; version 1,2 missing (must have been short lived...)

(test (decryption-test-version3 :depends-on key-tests)
  "Test decryption of version 3 against Turtl's tcrypt library."
  (let* ((key (key-from-string "HsKTwqcAdzAXSsK2Z8OaOy4RI8OqKnoUw5Miw7BbJcOUT8KOdcO0JwsO"))
         (tcrypt-ciphertext (from-base64 "AAOpckDeBymudt1AnCMpNUWE/3gA53BFCXVfl5eRXR6h2gQAAAAAmF5Li7QHzaJda8AwGom/ZGcFhKUjE9VOot2xxxKgQNop6MOkMq6stbbARt8ltbsVQb8I5wSTddcGUJapB6Spd/O+lZ7neYVBNttIm+kb3mekW4AjSBrNBFGpfqsGzOBp3ZVVpkUBJwlCT3/ZJdUXU9KqFlbHq/1uNesiRtXEugTRM4rtKaWoOvPvFye6msGDIxecdjJjI2tSJv4mCvPqnenPw9HzGGDp6U1s9r/FWtdsGoRfxuDPtIKEzuXm4t4CjxMlx/83fOV7xxE4EneMRTOlRUf8MM0eqNkDqAeDK8YNtOmdJLs3XVXRYGvPvh6eR9WcemLbcliz1gqjEmpc+UTWLrL/XDlbDcCQ2RacvJLoEq6i5ogkMa7XTyjKGhrg"))
         (tcrypt-plaintext "{\"type\":\"link\",\"url\":\"http://viget.com/extend/level-up-your-shell-game\",\"title\":\"Level Up Your Shell Game\",\"text\":\"Covers movements, aliases, many shortcuts. Good article.\\n\\nTODO: break out the useful bits into individual notes =]\",\"tags\":[\"bash\",\"shell\"],\"sort\":12}")
         (turtl-plaintext (babel:octets-to-string (decrypt key tcrypt-ciphertext))))
    (is (string= tcrypt-plaintext turtl-plaintext))))

(test (decryption-test-version4 :depends-on key-tests)
  "Test decryption of version 4 against Turtl's tcrypt library."
  (let* ((key (key-from-string "rYMgzeHsjMupzeUvqBpbsDRO1pBk/JWlJp9EHw3yGPs="))
         (tcrypt-ciphertext (from-base64 "AAQRk+ROrg4uNqRIwlAJrPOlTupLliAXexfnZpBt2nsCAAQAAAAA8PxvFb3rlGm50n75m4q7aLkif54G7BMiK1cqOAgKIziV7cN3Hyq+d2DggAkpSjnfcJXDDi60SGM+y0kjLUWOIuq0QVOFVF+c9OlhL6eQ5NsgYAr2ElUatg7jwufGbbCS93vItWssCJ3M5h2PTtaHTLtxhI0IrThkqeQYkV7bvK5tKOvo60Vc4pZ0LdAKfulIp3DJ0tmC15Nab2QVNDrQ35WB0tXZIBnloLIG0AkrBZYE+ig7cYK24QM52Z2sPSSQB33cKVe7U4OOZuS4rXBc1xwAhWKom9NZSMTYg6Ke69H4ZZTILZkkW4Qkgt+yIIJf"))
         (tcrypt-plaintext "{\"type\":\"link\",\"url\":\"http://www.youtube.com/watch?v=m_0e9VGO05g\",\"title\":\"M-Seven - Electronic Flip - YouTube\",\"text\":\"![image](http://i1.ytimg.com/vi/m_0e9VGO05g/maxresdefault.jpg)  \\n\",\"tags\":[\"electronic\",\"calm\"]}")
         (turtl-plaintext (babel:octets-to-string (decrypt key tcrypt-ciphertext))))
    (is (string= tcrypt-plaintext turtl-plaintext))))

(test (decryption-test-version5 :depends-on key-tests)
  "Test decryption of version 5 against Turtl's tcrypt library."
  (let* ((key (key-from-string "js8BsJMw2jeqdB/NoidMhP1MDwxCF+XUYf3b+r0fTXs="))
         (tcrypt-ciphertext (from-base64 "AAUCAAFKp4T7iuwnVM6+OY+nM9ZKnxe5sUvA+WvkVfhAFDYUKQRenSi+m1WCGGWZIigqL12fAvE4AA10MGACEjEbHvxGQ45qQcEiVZuqB3EMgijklXjNA+EcvnbvcOFSu3KJ4i1iTbsZH+KORmqoxGsWYXafq/EeejAMV94umfC0Uwl6cuCOav2OcDced5GYHwkd9qSDTR+SJyjgAq33r7ylVQQASa8YUP7jx/FGoT4mzjp0+rNAoyqGYU7gJz4v0ccUfm34ww1eZS1kkWmy33h5Cqi6R7Y0y57ytye2WXjvlHGC2/iglx7sPgemDCmIoIMBVdXDW5sMw2fxmpQph1pyst10+Wbv4DcNtN+fMpseDdmTpbXarGNFqyYul/QXM9WmzUTtjeW3kZxol989l+1WXrr6E5Ctk61NVb98PtRMFuRHYU8kt3cTUE4m0G8PGwK62vkp+2pI6fn3UijOFLYHDpeGbgRiAeBbykHgXrtXfAIpysl/FOl1NzfFz44="))
         (tcrypt-plaintext "{\"type\":\"link\",\"url\":\"http://www.baynatives.com/plants/Erysimum-capitatum/\",\"title\":\"Erysimum capitatum Gallery - Bay Natives: The San Francisco Source for Native Plants\",\"text\":\"![image](http://www.baynatives.com/plants/Erysimum-capitatum/03-P4082478__.jpg)  \\n\",\"tags\":[\"backyard\",\"garden\",\"native plants\",\"bay natives\",\"flower\",\"wildflower\"]}")
         (turtl-plaintext (babel:octets-to-string (decrypt key tcrypt-ciphertext))))
    (is (string= tcrypt-plaintext turtl-plaintext))))

