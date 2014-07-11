(in-package :turtl-core-test)
(in-suite turtl-core-model-user)

(test key-generation
  "Make sure user model generates proper keys (compared with JS user model)."
  (let* ((key-js (from-base64 "oLTnUzPogQ+HixDcVNZU5mFwzDhWNj2r6Ayy1tMSMIM="))
         (user (create-model 'user '(:username "slappy"
                                     :password "please dont guess me!")))
         (key-lisp (generate-key user)))
    (is (equalp key-js key-lisp))))

(test auth-generation
  "Test generating the original auth tokens (compared with JS)."
  (let* ((auth-js "bv+f8HySAzEU+8RCslsKXXCpOeKb9+pOcsbnTa3Uz1itfw9Hjjdim8ObYePUFjeCSExYElupJOklDLKsSMsTSZRH2DKUWnv7pDfXXHI6Ogg=:i6f6d6734633238313938373234396265")
         (user (create-model 'user '(:username "omg"
                                     :password "wookie")))
         (auth-lisp (generate-auth user)))
    (is (string= auth-js auth-lisp))))

