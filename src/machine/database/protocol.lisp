(cl:in-package #:huginn.machine.database)


(defvar *database*)

(defun database ()
  *database*)

(defmacro with-database ((database) &body body)
  `(let ((*database* ,database))
     ,@body))

(defclass fundamental-database ()
  ())

(defgeneric clauses* (database))

(defun clauses ()
  (clauses* (database)))

(defgeneric add-clause* (database clause))

(defun add-clause (clause)
  (add-clause* (database) clause))

(defgeneric make-database (class
                           &optional default
                           &rest more-options))
