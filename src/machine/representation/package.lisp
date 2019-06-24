(cl:in-package #:cl-user)


(defpackage huginn.machine.representation
  (:use #:huginn.aux-package #:cl)
  (:nicknames #:huginn.m.r)
  (:export
   #:tag-case
   #:+variable+
   #:+reference+
   #:+fixnum+
   #:+expression+
   #:variable-cell-p
   #:reference-cell-p
   #:fixnum-cell-p
   #:expression-cell-p
   #:same-cells-p
   #:variable-unbound-p
   #:make-reference
   #:execution-stack-cell
   #:execution-stack-cell-clause
   #:execution-stack-cell-clausese
   #:execution-stack-cell-goals
   #:execution-stack-cell-heap-pointer
   #:execution-stack-cell-heap-cells-trail
   #:execution-stack-cell-bindings-fill-pointer
   #:execution-stack-cell-heap-fill-pointer
   #:execution-stack-cell-previous-cell
   #:make-execution-stack-cell
   #:execution-state
   #:make-execution-state
   #:execution-state-clauses
   #:execution-state-objects-mapping
   #:execution-state-objects-variable-bindings
   #:execution-state-objects-heap
   #:execution-state-objects-unification-stack
   #:execution-state-objects-stack
   #:execution-stack-cell-more-goals-p
   #:clause-content-length
   #:make-clause
   #:clause
   #:clause-goal-pointers
   #:clause-variable-values
   #:clause-content
   #:clause-body-pointer
   #:make-clause
   #:clause-goals
   #:clause-body-length
   #:expand-state-heap
   #:dereference-variable
   #:execution-state-heap-size
   #:clause-length
   #:follow-pointer
   #:dereference-heap-pointer
   ))
