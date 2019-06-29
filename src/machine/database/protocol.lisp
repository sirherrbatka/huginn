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

(defgeneric matching-clauses* (database execution-state goal-pointer))

(defun matching-clauses (execution-state goal-pointer)
  (matching-clauses* (database) execution-state goal-pointer))

(defun clauses ()
  (clauses* (database)))

(defgeneric add-clause* (database clause))

(defun add-clause (clause)
  (add-clause* (database) clause))

(defgeneric make-database (class
                           &optional default
                           &rest more-options))

(defgeneric clear* (database))

(defun clear ()
  (clear* (database)))
