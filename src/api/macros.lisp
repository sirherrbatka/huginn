(cl:in-package #:huginn)


(defmacro ?- (&body goals)
  `())


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))
