(cl:in-package #:huginn)


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))


(defmacro with-options ((&key (compile nil) (database '*database*))
                        &body body)
  `(let ((*compile* ,compile)
         (*database* ,database))
     ,@body))
