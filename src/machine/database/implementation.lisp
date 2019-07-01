(cl:in-package #:huginn.machine.database)


(defclass database (fundamental-database)
  ((%clauses :initarg :clauses
             :reader clauses))
  (:default-initargs :clauses (vect)))


(defmethod add-clause ((database database) clause)
  (vector-push-extend clause (clauses database)))


(defmethod make-database ((class (eql 'database))
                          &rest more-options)
  (apply #'make 'database more-options))


(defmethod matching-clauses ((database database)
                              execution-state
                              goal-pointer)
  (declare (type huginn.m.r:execution-state execution-state)
           (type huginn.m.r:pointer goal-pointer)
           (optimize (debug 3)))
  (bind (((:flet deref (i))
          (aref (huginn.m.r:execution-state-heap execution-state) i))
         (arity (deref (1+ goal-pointer)))
         (id (deref goal-pointer))
         (buffer (make-array arity
                             :element-type 'huginn.m.r:pointer)))
    (assert (huginn.m.r:expression-cell-p id))
    (assert (not (zerop arity)))
    (iterate
      (for i from 0 below arity)
      (setf (aref buffer i) (deref (+ goal-pointer 2 i))))
    (~> database
        clauses
        cl-ds:whole-range
        (cl-ds.alg:only
         (lambda (clause)
           (let ((content (huginn.m.r:clause-content clause)))
             (and (eql (aref content 1) arity) ; same arity
                  (let ((clause-predicate (aref content 2))
                        (goal-predicate (aref buffer 1)))
                    (declare (ignore clause-predicate goal-predicate))
                    ;; I need another tag type: predicate and index those indepenendtly from everything else
                    t))))))))

(defmethod clear ((database database))
  (setf (fill-pointer (clauses database)) 0))
