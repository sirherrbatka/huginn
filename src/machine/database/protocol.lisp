(cl:in-package #:huginn.machine.database)


(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))

(defclass fundamental-database ()
  ())

(defclass id-tagged-object ()
  ((%id
    :initform (gensym)
    :reader id)))

(defgeneric clauses (database))

(defgeneric matching-clauses (database execution-state goal-pointer clause))

(defgeneric index-predicate (database predicate))

(defgeneric predicate-from-cell/word (database cell/word))

(defgeneric add-clause (database clause))

(defgeneric make-database (class &rest more-options))

(defgeneric clear (database))

(defgeneric word (condition))

(defgeneric id (object))

(defgeneric register-object (database object))
