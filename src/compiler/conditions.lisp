(cl:in-package #:huginn.compiler)


(defun has-predicate-p (expression)
  cl-ds.utils:todo)


(defun goalp (expression)
  cl-ds.utils:todo)


(define-condition invalid-goal (program-error)
  ())


(define-condition invalid-predicate (program-error)
  ())

#|

* TODO Validated if predicate is present, signal condition if not.
* TODO Validate if each goal is proper.

|#
