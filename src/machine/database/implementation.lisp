(cl:in-package #:huginn.machine.database)


(defclass database (fundamental-database)
  ((%clauses :initarg :clauses
             :reader clauses)
   (%predicates :initarg :predicates
                :accessor access-predicates)
   (%predicates-index :initarg :predicates-index
                      :accessor access-predicates-index))
  (:default-initargs
   :clauses (vect)
   :predicates-index 1
   :predicates (make-hash-table)))


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
  (bind ((heap (huginn.m.r:execution-state-heap execution-state))
         ((:flet deref (i))
          (aref heap i))
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
                        (goal-predicate (aref buffer 0)))
                    (or (huginn.m.r:predicate-unbound-p goal-predicate)
                        (huginn.m.r:same-cells-p goal-predicate
                                                 clause-predicate))))))))))


(defmethod clear ((database database))
  (setf (fill-pointer (clauses database)) 0
        (access-predicates-index database) 1
        (access-predicates database) (make-hash-table)))


(defmethod index-predicate ((database database) predicate)
  (check-type predicate symbol)
  (lret ((result (ensure (gethash predicate (access-predicates database))
                   #1=(access-predicates-index database))))
    (when (= result #1#)
      (incf #1#))))
