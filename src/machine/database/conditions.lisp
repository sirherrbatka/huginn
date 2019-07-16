(cl:in-package #:huginn.machine.database)


(define-condition predicate-dereference-error
    (error more-conditions:chainable-condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Could not dereference predicate."))))


(define-condition predicate-unbound-error (predicate-dereference-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Could not dereference predicate because predicate remains unbound."))))


(define-condition unknown-predicate-error (predicate-dereference-error)
  ((cell :initarg :cell
         :reader cell))
  (:report (lambda (condition stream)
             (let* ((cell (cell condition))
                    (word (huginn.m.r:detag cell)))
               (format stream "Unknown predicate cell ~a (word: ~a). This error indicates bug in the huginn database itself."
                       cell
                       word)))))
