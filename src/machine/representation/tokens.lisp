(cl:in-package #:huginn.machine.representation)


(defmacro define-tokens (&body tokens)
  `(progn
     ,@(mapcar (let ((i 0))
                 (lambda (name)
                   (prog1 `(define-constant ,name ,i)
                     (incf i))))
               tokens)))
