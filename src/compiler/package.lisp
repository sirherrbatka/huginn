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
   #:optimized-relocate-cells-function
   #:optimized-unify-head-function
   #:pointer-for-expression
   #:pointer-for-list
   #:pointer-for-predicate
   #:pointer-for-variable
   #:predicate
   #:predicates
   #:variable-bindings
   #:variables))
