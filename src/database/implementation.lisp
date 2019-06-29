(cl:in-package #:huginn.database)


(defclass database (fundamental-database)
  ((%clauses :initarg :clauses
             :reader clauses*))
  (:default-initargs :clauses (vect)))


(defmethod add-clause* ((database database) clause)
  (vector-push-extend clause (clauses* database)))


(defmethod make-database ((class (eql 'database))
                          &optional default
                          &rest more-options)
  (lret ((result (apply #'make 'database more-options)))
    (when default (setf *database* result))))
