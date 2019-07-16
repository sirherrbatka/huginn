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
  ((word :initarg :word
         :reader word))
  (:report (lambda (condition stream)
             (let* ((word (word condition)))
               (format stream "Unknown predicate word ~a. This error indicates bug in the huginn database itself."
                       word)))))
