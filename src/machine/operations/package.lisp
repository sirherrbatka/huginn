(cl:in-package #:cl-user)


(defpackage #:huginn.machine.operations
  (:use #:cl #:huginn.aux-package)
  (:nicknames #:huginn.m.o)
  (:export
   #:find-answer
   #:pop-stack-cells-until-goal
   #:pop-stack-cell
   #:pop-stack-cells-until-possible-answer
   #:index-object
   ))
