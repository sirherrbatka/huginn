(cl:in-package #:huginn.compiler)


(defun has-predicate-p (expression)
  cl-ds.utils:todo)


(defun goalp (expression)
  cl-ds.utils:todo)


(define-condition compiler-error (program-error)
  ((%form :reader read-form
          :initarg :form)))


(define-condition invalid-goal (compiler-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid goal in the form ~a."
                     (read-form condition)))))


(define-condition invalid-predicate (program-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid predicate ~a."
                     (read-form condition)))))

#|

* TODO Validated if predicate is present, signal condition if not.
* TODO Validate if each goal is proper.

|#
