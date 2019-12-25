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
   :clauses (make-hash-table :test 'equal)
   :reverse-predicate (vect)
   :predicates-index 1
   :predicates (make-hash-table)))


(defmethod add-clause ((database database) clause)
  (let* ((content (huginn.m.r:clause-content clause))
         (expression-cell (aref content 0))
         (expression-position (huginn.m.r:detag expression-cell))
         (arity-cell (aref content expression-position))
         (predicate-cell (aref content (1+ expression-position))))
    (assert (huginn.m.r:predicate-cell-p predicate-cell))
    (assert (huginn.m.r:fixnum-cell-p arity-cell))
    (vector-push-extend
     clause
     (ensure (gethash (cons predicate-cell arity-cell)
                      (clauses database))
       (vect)))))


(defmethod make-database ((class (eql 'database))
                          &rest more-options)
  (apply #'make 'database more-options))


(def empty-range (make 'cl-ds:empty-range))


(defmethod matching-clauses ((database database)
                             execution-state
                             goal-pointer
                             clause)
  (declare (type huginn.m.r:execution-state execution-state)
           (type huginn.m.r:pointer goal-pointer)
           (optimize (speed 3) (safety 0) (space 0)
                     (debug 0) (compilation-speed 0)))
  (bind (((:flet deref (i))
          (declare (type huginn.m.r:pointer i))
          (~> execution-state
              huginn.m.r:execution-state-heap
              (aref i)))
         (expression (deref goal-pointer))
         (expression-position (huginn.m.r:detag expression))
         (arity-cell (~> expression-position deref))
         (predicate-cell (~> expression-position 1+ deref))
         (clauses (clauses database))
         (result-vector (gethash (cons predicate-cell arity-cell) clauses))
         (length (if (null result-vector)
                     0
                     (fill-pointer result-vector))))
    (declare (type hash-table clauses)
             (type (or null (vector t)) result-vector)
             (type fixnum length))
    (assert (huginn.m.r:expression-cell-p expression))
    (assert (huginn.m.r:fixnum-cell-p arity-cell))
    (assert (huginn.m.r:predicate-cell-p predicate-cell))
    (assert (and result-vector (array-has-fill-pointer-p result-vector)))
    ;; TODO, this will not handle unbound predicates
    (if (zerop length)
        empty-range
        (make-clauses-range clause result-vector))))


(defmethod clear ((database database))
  (~> database clauses clrhash)
  (setf (~> database access-reverse-predicates fill-pointer) 0
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
    (when (zerop word)
      (error 'predicate-unbound-error))
    (unless (< index length)
      (error 'unknown-predicate-error :word word))
    (aref predicates index)))
