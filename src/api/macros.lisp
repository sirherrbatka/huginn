(cl:in-package #:huginn)


(defmacro <- (&body clause)
  `())


(defmacro ?- (&body goals)
  `())


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))
