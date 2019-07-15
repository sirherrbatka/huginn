(cl:in-package #:cl-user)


(defpackage #:huginn.machine.database
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.m.d)
  (:export
   #:add-clause
   #:matching-clauses
   #:clear
   #:clauses
   #:database
   #:index-predicate
   #:predicate-from-cell/word
   #:fundamental-database
   #:make-database
   #:with-database
   #:database
   ))
