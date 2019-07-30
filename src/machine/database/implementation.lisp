(cl:in-package #:huginn.machine.database)


(defclass database (fundamental-database)
  ((%clauses :initarg :clauses
             :reader clauses)
   (%reverse-predicate :initarg :reverse-predicate
                       :accessor access-reverse-predicates)
   (%predicates :initarg :predicates
                :accessor access-predicates)
   (%predicates-index :initarg :predicates-index
                      :accessor access-predicates-index))
  (:default-initargs
   :clauses (vect)
   :reverse-predicate (vect)
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
  (bind (((:flet deref (i))
          (~> execution-state
              huginn.m.r:execution-state-heap
              (aref i)))
         (expression (deref goal-pointer))
         (expression-position (huginn.m.r:detag expression))
         (arity-cell (~>  expression-position huginn.m.r:detag deref))
         (arity (huginn.m.r:detag arity-cell)))
    (assert (huginn.m.r:expression-cell-p expression))
    (assert (huginn.m.r:fixnum-cell-p arity-cell))
    (assert (> arity 1))
    (~> database
        clauses
        cl-ds:whole-range
        (cl-ds.alg:only
         (lambda (clause)
           (let* ((content (huginn.m.r:clause-content clause))
                  (expression (first-elt content))
                  (p (huginn.m.r:detag expression)))
             (assert (huginn.m.r:expression-cell-p expression))
             (and (eql (huginn.m.r:detag (aref content p)) arity) ; same arity
                  (let ((clause-predicate (aref content (1+ p)))
                        (goal-predicate (deref (1+ expression-position))))
                    (or (huginn.m.r:predicate-unbound-p goal-predicate)
                        (huginn.m.r:same-cells-p goal-predicate
                                                 clause-predicate))))))))))


(defmethod clear ((database database))
  (setf (~> database clauses fill-pointer) 0
        (~> database access-reverse-predicates fill-pointer) 0
        (access-predicates-index database) 1
        (access-predicates database) (make-hash-table)))


(defmethod index-predicate ((database database) predicate)
  (check-type predicate symbol)
  (lret ((result (ensure (gethash predicate (access-predicates database))
                   #1=(access-predicates-index database))))
    (when (= result #1#)
      (vector-push-extend predicate (access-reverse-predicates database))
      (incf #1#))))


(defmethod predicate-from-cell/word ((database database)
                                     cell/word)
  (check-type cell/word huginn.m.r:cell)
  (let* ((word (huginn.m.r:detag cell/word))
         (index (1- word))
         (predicates (access-reverse-predicates database))
         (length (length predicates)))
    (when (zerop index)
      (error 'predicate-unbound-error))
    (unless (< index length)
      (error 'unknown-predicate-error :word word))
    (aref predicates index)))
