(cl:in-package #:cl-user)


(defpackage #:huginn.compiler
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.c)
  (:shadow cl:variable)
  (:export
   #:body
   #:body-pointer
   #:compilation-state
   #:content
   #:expression
   #:head
   #:pointer-for-expression
   #:pointer-for-variable
   #:predicate
   #:variable-bindings
   #:fundamental-compilation-state))
