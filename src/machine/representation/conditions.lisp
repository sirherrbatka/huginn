(cl:in-package #:huginn.machine.representation)


(define-condition variable-dereference-error
    (error more-conditions:chainable-condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Could not dereference variable."))))


(define-condition variable-unbound-error (variable-dereference-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Could not dereference variable because predicate remains unbound."))))


(define-condition unknown-variable-error (variable-dereference-error)
  ((cell :initarg :cell
         :reader cell))
  (:report (lambda (condition stream)
             (let* ((cell (cell condition))
                    (word (huginn.m.r:detag cell)))
               (format stream "Unknown variable cell ~a (word: ~a). This error indicates bug in the huginn itself."
                       cell
                       word)))))
