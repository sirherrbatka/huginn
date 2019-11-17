(cl:in-package #:cl-user)


(defpackage #:huginn.compiler
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.c)
  (:shadow cl:variable)
  (:export
   #:body
   #:body-pointer
   #:cells-count
   #:compilation-state
   #:content
   #:expression
   #:expressions
   #:fundamental-compilation-state
   #:head
   #:invalid-goal
   #:invalid-predicate
   #:list-input
   #:make-compilation-state
   #:recursive-call
   #:multiple-recursive-goals
   #:recursive-p
   #:optimized-relocate-cells-function
   #:multiple-goals-in-recursive-clause
   #:optimized-unify-head-function
   #:pointer-for-expression
   #:pointer-for-list
   #:pointer-for-recursive-goal
   #:pointer-for-predicate
   #:pointer-for-variable
   #:predicate
   #:predicates
   #:variable-bindings
   #:variables))
