(cl:in-package #:cl-user)


(defpackage #:huginn.machine.representation
  (:use #:huginn.aux-package #:cl)
  (:nicknames #:huginn.m.r)
  (:export
   #:+all-tags+
   #:+expression+
   #:+fixnum+
   #:+list-end+
   #:+list-rest+
   #:+list-start+
   #:+predicate+
   #:+reference+
   #:+tag-size+
   #:+variable+
   #:cell
   #:cell
   #:clause
   #:clause-body-length
   #:clause-body-pointer
   #:clause-content
   #:clause-content-length
   #:clause-copy-body-function
   #:clause-copy-head-function
   #:clause-goal-pointers
   #:clause-goals
   #:clause-input
   #:clause-length
   #:clause-recursive-call-position
   #:clause-recursive-p
   #:clause-unify-head-function
   #:clause-variable-values
   #:clone-execution-stack-cell
   #:clone-execution-state
   #:dereference-heap-pointer
   #:dereference-variable
   #:detag
   #:execution-stack-cell
   #:execution-stack-cell-bindings-fill-pointer
   #:execution-stack-cell-clause
   #:execution-stack-cell-clauses
   #:execution-stack-cell-clausese
   #:execution-stack-cell-goals
   #:execution-stack-cell-goal-pointer
   #:execution-stack-cell-heap-fill-pointer
   #:execution-stack-cell-heap-pointer
   #:execution-stack-cell-more-goals-p
   #:execution-stack-cell-p
   #:execution-stack-cell-previous-cell
   #:execution-stack-cell-recursive-call-position
   #:execution-stack-cell-same-clause-p
   #:execution-stack-cell-unwind-trail-pointer
   #:execution-state
   #:execution-state-database
   #:execution-state-heap
   #:execution-state-heap-size
   #:execution-state-objects-mapping
   #:execution-state-stack
   #:execution-state-unification-stack
   #:execution-state-unification-stack-fill-pointer
   #:execution-state-unwind-trail
   #:execution-state-variable-bindings
   #:expand-state-heap
   #:expression-cell-p
   #:fixnum-cell-p
   #:follow-pointer
   #:index-object
   #:list-end-cell-p
   #:list-rest-cell-p
   #:list-rest-unbound-p
   #:list-start-cell-p
   #:make-clause
   #:make-execution-stack-cell
   #:make-execution-state
   #:make-initial-execution-stack-cell
   #:make-reference
   #:pointer
   #:predicate-cell-p
   #:predicate-unbound-p
   #:recursive-execution-stack-cell-p
   #:reference-cell-p
   #:retag
   #:same-cells-p
   #:scan-heap-list
   #:symbol-tag-of
   #:tag
   #:tag-case
   #:tag-of
   #:unknown-variable-error
   #:variable-cell-p
   #:variable-dereference-error
   #:variable-unbound-error
   #:variable-unbound-p
   #:word
   ))
