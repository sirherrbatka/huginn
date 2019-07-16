(cl:in-package #:huginn.machine.database)


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))

(defclass fundamental-database ()
  ())

(defgeneric clauses (database))

(defgeneric matching-clauses (database execution-state goal-pointer))

(defgeneric index-predicate (database predicate))

(defgeneric predicate-from-cell/word (database cell/word))

(defgeneric add-clause (database clause))

(defgeneric make-database (class &rest more-options))

(defgeneric clear (database))

(defgeneric cell (condition))
