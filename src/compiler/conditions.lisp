(cl:in-package #:huginn.compiler)


(defun has-predicate-p (expression)
  (check-type expression clause)
  (let ((predicate
          (clause-head-predicate expression)))
    (typep predicate 'predicate)))


(defun has-at-least-one-argument-p (expression)
  (and (> (length expression) 2)
       (some #'variablep (rest expression))))


(defun goalp (expression)
  (and (expressionp expression)
       (has-predicate-p expression)
       (has-at-least-one-argument-p expression)))


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
