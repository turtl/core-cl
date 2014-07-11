(in-package :turtl-core-test)
(in-suite turtl-core-protected)

(defclass note (protected)
  ((public-fields :accessor public-fields :initform '("id"))
   (private-fields :accessor private-fields :initform '("text"))))

(test deserialize
  "Test deserializing `body` field in models."
  (let ((key (from-base64 "js8BsJMw2jeqdB/NoidMhP1MDwxCF+XUYf3b+r0fTXs="))
        (data (yason:parse "{\"board_id\":\"51c67628d6d59353f5000004\",\"keys\":[{\"b\":\"51c67628d6d59353f5000004\",\"k\":\"AAUCAAF2Q/KkCzP6UhP9OE1GUUvXZ7MhfN3OQQ7Sto0ok4xwQfOtBTf0aGmZA01qiJSJI63LE3MbzQtCqUyaSRiql9t3FA==\"}],\"user_id\":\"51c67620d6d59353f5000003\",\"id\":\"534b52fd2b13755d0b00eb5d\",\"mod\":1397445400,\"meta\":{\"persona\":\"5229015fd6d5934932000002\"},\"body\":\"AAUCAAFOH09Fot0b8ePf6qDyOJT/hUEbmUvZh9Uoq2/AqHKMyd/cT6mxiJ+tkKl8Oj4r2UISOizH4ikms9gt+qloWpvRKtNTE0C1rsySpYReR69tKT7VmlR9NaOrB2UXxqwLfNHLyHOaRSo/x0EUR16IkERwKqf1nf2Ns9zGaxUCk50Id1FT//97gsgO5Dc9fbdVIsb0ZorRVWLzheJPBLM83QVym4NGBc5IgqLuElWomsnNddsJ3C2fRyBOYl4IG+zurXyhNeb7+pH78EdhAWhkbp6Imb7GOt8s5QntRKfHeDthI3JCsKZhFaJbLK/Db+vRfL/r4KHv5voZ6kDlZZZvRQxHsgDzGU3jLRKZNz/5cgCLBnxQSpKHMf9F7sVCP1+fXJoDAN8E9JIiqc0hlq8J/K8jKphFvgGl77rZh1RQPMe6b1hYzDaOjg9Ra0TrbPKB7dmaMky+JfyFwuvZlBq7LLq9UovTrTNfkscvzoIhPsE17SEpUMkMI/bPvbM=\"}"))
        (model (create-model 'note nil))
        (deserialized nil)
        (err nil))
    (setf (key model) key)
    (with-running-test
      (future-handler-case
        (wait-for (mset model data)
          (setf deserialized (mget model "tags")))
        (t (e)
          (format t "err: ~a~%" e)
          (setf err e))))
    (is (eq nil err))
    (is (find "flower" deserialized :test 'string=))))

(test serialize
  "Test serializing a model's private fields."
  (let ((key (from-base64 "84Jpfyn+srpGYJZ/ndZQVlwxqBjjlq24+aQBERe3gjY="))
        (model (create-model 'note '(:text "Sha na na na na, sha na na na. (what the hell are you doing) Get a job.")))
        (serialized nil)
        (err nil))
    (setf (key model) key)
    (with-running-test
      (future-handler-case
        (alet ((ser (mserialize model)))
          (setf serialized ser))
        (t (e) (setf err e))))
    (is (eq nil err))
    (is (stringp (gethash (body-key model) serialized)))
    (let ((dec (decrypt (key model) (from-base64 (gethash (body-key model) serialized)))))
      (is (string= "{\"text\":\"Sha na na na na, sha na na na. (what the hell are you doing) Get a job.\"}"
                   (babel:octets-to-string dec))))))

(test madd-async
  "Test adding a model to a collection async."
  (let ((data (hu:hash ("name" "vince") ("advice" "you've got to accessorize, howard.")))
        (collection (create-collection 'collection))
        (count nil)
        (err nil))
    (with-running-test
      (future-handler-case
        (wait-for (madd-async collection data)
          (setf count (length (models collection))))
        (t (e) (setf err e))))
    (is (= 1 count))
    (is (null err))))

(test (mreset-async :depends-on madd-async)
  "Test resetting data into a collection async."
  (let ((items (loop for i from 0 to 9 collect (hu:hash ("age" i))))
        (collection (create-collection 'collection))
        (count nil)
        (err nil))
    (with-running-test
      (future-handler-case
        (wait-for (mreset-async collection items)
          (setf count (length (models collection))))
        (t (e) (setf err e))))
    (is (= 10 count))
    (is (null err))))

(test (mreset-async-noitems :depends-on mreset-async)
  "Test that resetting and empty data set async into a collection actually
   finishes the future it's supposed to."
  (let ((collection (create-collection 'collection))
        (err nil)
        (is-set nil))
    (with-running-test
      (future-handler-case
        (wait-for (mreset-async collection (list))
          (setf is-set t))
        (t (e) (setf err e))))
    (is (eq t is-set))
    (is (null err))))

