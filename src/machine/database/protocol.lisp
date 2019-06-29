(cl:in-package #:huginn.machine.database)


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))

(defclass fundamental-database ()
  ())

(defgeneric clauses (database))

(defgeneric matching-clauses (database execution-state goal-pointer))

(defgeneric add-clause (database clause))

(defgeneric make-database (class
                           &optional default
                           &rest more-options))

(defgeneric clear (database))
