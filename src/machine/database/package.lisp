(cl:in-package #:cl-user)


(defpackage #:huginn.machine.database
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.m.d)
  (:export
   #:add-clause
   #:word
   #:id
   #:id-tagged-object
   #:objects-database
   #:register-object
   #:clauses
   #:clear
   #:database
   #:database
   #:fundamental-database
   #:index-predicate
   #:make-database
   #:matching-clauses
   #:predicate-dereference-error
   #:predicate-from-cell/word
   #:predicate-unbound-error
   #:unknown-predicate-error
   #:with-database
   ))
