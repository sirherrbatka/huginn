(cl:in-package #:cl-user)


(defpackage #:huginn.machine.representation
  (:use #:huginn.aux-package #:cl)
  (:nicknames #:huginn.m.r)
  (:export
   #:+expression+
   #:+fixnum+
   #:+reference+
   #:+tag-size+
   #:+variable+
   #:cell
   #:clause
   #:clause-body-length
   #:clause-body-pointer
   #:clause-content
   #:clause-content-length
   #:clause-goal-pointers
   #:clause-goals
   #:clause-length
   #:clause-variable-values
   #:dereference-heap-pointer
   #:dereference-variable
   #:detag
   #:execution-stack-cell
   #:execution-stack-cell-bindings-fill-pointer
   #:execution-stack-cell-clause
   #:execution-stack-cell-clauses
   #:execution-stack-cell-clausese
   #:execution-stack-cell-goals
   #:execution-stack-cell-heap-cells-trail
   #:execution-stack-cell-heap-fill-pointer
   #:execution-stack-cell-heap-pointer
   #:execution-stack-cell-more-goals-p
   #:execution-stack-cell-previous-cell
   #:execution-state
   #:execution-state-clauses
   #:execution-state-heap
   #:execution-state-heap-size
   #:execution-state-objects-mapping
   #:execution-state-stack
   #:execution-state-unification-stack
   #:execution-state-variable-bindings
   #:expand-state-heap
   #:expression-cell-p
   #:fixnum-cell-p
   #:follow-pointer
   #:index-object
   #:make-clause
   #:make-clause
   #:make-execution-stack-cell
   #:make-execution-state
   #:make-reference
   #:pointer
   #:reference-cell-p
   #:same-cells-p
   #:tag
   #:tag-case
   #:tag-of
   #:variable-cell-p
   #:variable-unbound-p
   #:word))
