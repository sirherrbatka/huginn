(cl:in-package #:cl-user)


(defpackage #:huginn
  (:use #:cl #:huginn.aux-package)
  (:export
   #:<-
   #:?-
   #:add-clause
   #:clauses
   #:clear
   #:li
   #:compiler
   #:cant-bind-predicate-error
   #:cant-bind-variable-error
   #:cant-bind-error
   #:database
   #:make-database
   #:with-database
   #:clauses))


(cl:defpackage #:huginn-user
  (:use #:huginn #:cl))
