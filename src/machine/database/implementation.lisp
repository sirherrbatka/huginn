(cl:in-package #:huginn.machine.database)


(defclass database (fundamental-database)
  ((%clauses :initarg :clauses
             :reader clauses))
  (:default-initargs :clauses (vect)))


(defmethod add-clause ((database database) clause)
  (vector-push-extend clause (clauses* database)))


(defmethod make-database ((class (eql 'database))
                          &rest more-options)
  (apply #'make 'database more-options))


(defmethod matching-clauses ((database database)
                              execution-state
                              goal-pointer)
  (declare (type huginn.m.r:execution-state execution-state)
           (type huginn.m.r:pointer goal-pointer))
  (let ((heap (huginn.m.r:execution-state-heap execution-state)))
    (macrolet ((deref (i)
                 `(aref heap ,i)))
      (~> database
          clauses*
          cl-ds:whole-range
          (cl-ds.alg:only
           (lambda (clause)
             (let ((content (huginn.m.r:clause-content clause)))
               (and (eql (aref content 1)
                         (deref (1+ goal-pointer))) ; same arity
                    (let ((clause-predicate (aref content 2))
                          (goal-predicate (deref (+ 2 goal-pointer))))
                      (declare (ignore clause-predicate goal-predicate))
                      ;; I need another tag type: predicate and index those indepenendtly from everything else
                      t)))))))))

(defmethod clear ((database database))
  (setf (fill-pointer (clauses* database)) 0))
