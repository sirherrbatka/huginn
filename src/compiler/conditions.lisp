(cl:in-package #:huginn.compiler)


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


(define-condition multiple-recursive-goals (compiler-error)
  ())
