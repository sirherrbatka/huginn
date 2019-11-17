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
   #:variable-symbol
   #:database
   #:make-database
   #:with-database
   #:shared-resources
   #:*shared-resources*
   #:next-answer
   #:*compile*
   #:with-options
   #:make-shared-resources
   #:recur
   #:clauses))


(cl:defpackage #:huginn-user
  (:use #:huginn #:cl))
