(in-package :turtl-core-test)

(in-suite turtl-core-crypto)

(test key-tests
  "Test generation, (de)serialization or keys."
  ;; test key deserialization from base64 (values grabbed from tcrypt)
  (let ((key-str "csUNaLbDnnZitPgkqOvIu8k1EPru+liFBnoOclHfJnw=")
        (key-bytes #(114 197 13 104 182 195 158 118 98 180 248 36 168 235 200 187 201 53 16 250 238 250 88 133 6 122 14 114 81 223 38 124)))
    (is (equalp key-bytes (key-from-string key-str))))
  ;; test key serialization to base64
  (let ((key-bytes #(253 124 166 27 10 241 223 35 68 14 61 210 126 245 17 7 196 93 203 227 67 156 120 83 224 83 247 111 22 88 227 98))
        (key-str "/XymGwrx3yNEDj3SfvURB8Rdy+NDnHhT4FP3bxZY42I="))
    (is (equalp key-str (key-to-string key-bytes)))))

(test (simple-encryption-test :depends-on key-tests)
  "Tests encrypting versions 0-n (n being the current version) and correllating
   the results with the Turtl JS (tcrypt) output."
  ;; version 0
  (let* ((plaintext "i am going to encrypt this, hope it matches with tcrypt.")
         (key (key-from-string "oPCkqo99Egq1kvkYAGcL0LnovZORZ2SBpcT49Flf9yI="))
         (iv (
              


