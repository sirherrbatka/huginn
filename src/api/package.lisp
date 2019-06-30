(cl:in-package #:cl-user)


(defpackage huginn
  (:use #:cl #:huginn.aux-package)
  (:export
   #:<-
   #:?-
   #:add-clause
   #:clauses
   #:clear
   #:compiler
   #:database
   #:make-database
   #:with-database
   #:clauses))
