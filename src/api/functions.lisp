(cl:in-package #:huginn)


(defun compiler ()
  *compiler*)


(defun database ()
  *database*)


(defun (setf database) (database)
  (setf *database* database))


(defun (setf compiler) (compiler)
  (setf *compiler* compiler))


(defun add-clause (clause)
  (huginn.m.d:add-clause (database) clause))


(defun clauses ()
  (huginn.m.d:clauses (database)))


(defun clear ()
  (huginn.m.d:clear (database)))

(defun make-database (class &optional default &rest more-options)
  (lret ((result (apply #'huginn.m.d:make-database class more-options)))
    (when default
      (setf *database* result))))
