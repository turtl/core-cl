(in-package :turtl-core-test)

(in-suite turtl-core-protected)

(defun mtest ()
(let ((key (from-base64 "js8BsJMw2jeqdB/NoidMhP1MDwxCF+XUYf3b+r0fTXs="))
      (data (yason:parse "{\"board_id\":\"51c67628d6d59353f5000004\",\"keys\":[{\"b\":\"51c67628d6d59353f5000004\",\"k\":\"AAUCAAF2Q/KkCzP6UhP9OE1GUUvXZ7MhfN3OQQ7Sto0ok4xwQfOtBTf0aGmZA01qiJSJI63LE3MbzQtCqUyaSRiql9t3FA==\"}],\"user_id\":\"51c67620d6d59353f5000003\",\"id\":\"534b52fd2b13755d0b00eb5d\",\"mod\":1397445400,\"meta\":{\"persona\":\"5229015fd6d5934932000002\"},\"body\":\"AAUCAAFOH09Fot0b8ePf6qDyOJT/hUEbmUvZh9Uoq2/AqHKMyd/cT6mxiJ+tkKl8Oj4r2UISOizH4ikms9gt+qloWpvRKtNTE0C1rsySpYReR69tKT7VmlR9NaOrB2UXxqwLfNHLyHOaRSo/x0EUR16IkERwKqf1nf2Ns9zGaxUCk50Id1FT//97gsgO5Dc9fbdVIsb0ZorRVWLzheJPBLM83QVym4NGBc5IgqLuElWomsnNddsJ3C2fRyBOYl4IG+zurXyhNeb7+pH78EdhAWhkbp6Imb7GOt8s5QntRKfHeDthI3JCsKZhFaJbLK/Db+vRfL/r4KHv5voZ6kDlZZZvRQxHsgDzGU3jLRKZNz/5cgCLBnxQSpKHMf9F7sVCP1+fXJoDAN8E9JIiqc0hlq8J/K8jKphFvgGl77rZh1RQPMe6b1hYzDaOjg9Ra0TrbPKB7dmaMky+JfyFwuvZlBq7LLq9UovTrTNfkscvzoIhPsE17SEpUMkMI/bPvbM=\"}"))
      (model (create-model 'protected nil)))
  (setf (key model) key)
  (future-handler-case
    (alet ((des (mset model data)))
      (format t "des: ~a~%" (babel:octets-to-string des))
      (jprint (mserialize model)))
    (t (e) (format t "error: ~a~%" e))))
)


