(cl:in-package #:huginn)


(define-condition cant-bind-error (cl:error
                                   more-conditions:chainable-condition)
  ())


(define-condition cant-bind-variable-error (cant-bind-error)
  ((%variable-symbol :initarg :variable-symbol
                     :reader variable-symbol)))


(define-condition cant-bind-predicate-error (cant-bind-error)
  ())
