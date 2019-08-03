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
           (optimize (speed 3)))
  (bind (((:flet deref (i))
          (declare (type huginn.m.r:pointer i))
          (~> execution-state
              huginn.m.r:execution-state-heap
              (aref i)))
         (expression (deref goal-pointer))
         (expression-position (huginn.m.r:detag expression))
         (arity-cell (~>  expression-position huginn.m.r:detag deref))
         (arity (huginn.m.r:detag arity-cell))
         (clauses (clauses database))
         (length (fill-pointer clauses)))
    (declare (type clauses vector))
    (assert (huginn.m.r:expression-cell-p expression))
    (assert (huginn.m.r:fixnum-cell-p arity-cell))
    (assert (> arity 1))
    (cl-ds:xpr (:i 0)
      (when (< i length)
        (let* ((clause (aref clauses i))
               (content (huginn.m.r:clause-content clause))
               (expression (aref content 0))
               (p (huginn.m.r:detag expression)))
          (assert (huginn.m.r:expression-cell-p expression))
          (if (and (eql (huginn.m.r:detag (aref content p)) arity) ; same arity
                   (let ((clause-predicate (aref content (1+ p)))
                         (goal-predicate (deref (1+ expression-position))))
                     (or (huginn.m.r:predicate-unbound-p goal-predicate)
                         (huginn.m.r:same-cells-p goal-predicate
                                                  clause-predicate))))
              (cl-ds:send-recur clause :i (1+ i))
              (cl-ds:recur :i (1+ i))))))))


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
    (when (zerop word)
      (error 'predicate-unbound-error))
    (unless (< index length)
      (error 'unknown-predicate-error :word word))
    (aref predicates index)))
