(cl:in-package #:huginn)


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))
