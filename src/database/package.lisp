(cl:in-package #:cl-user)


(defpackage #:huginn.database
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.d)
  (:export
   #:add-clause
   #:clauses
   #:database
   #:fundamental-database
   #:make-database
   #:with-database
   #:database
   ))
