(cl:in-package #:cl-user)


(defpackage #:huginn.machine.database
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.m.d)
  (:export
   #:add-clause
   #:clauses
   #:database
   #:fundamental-database
   #:make-database
   #:with-database
   #:database
   ))
