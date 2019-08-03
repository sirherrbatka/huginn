(cl:in-package #:huginn)


(defvar *compiler* 'huginn.c:compilation-state)

(defvar *database*)

(defvar *shared-resources* nil)

(defstruct shared-resources
  (heap (make-array 64 :element-type 'huginn.m.r:cell)
   :type (simple-array huginn.m.r:cell (*)))
  (unification-stack (make-array 64 :element-type 'fixnum)
   :type (simple-array fixnum (*)))
  (unwind-trail (make-array 64 :element-type 'fixnum)
   :type (simple-array fixnum (*))))
