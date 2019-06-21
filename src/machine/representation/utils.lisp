(cl:in-package #:huginn.machine.representation)


(defmacro and (&body body)
  `(cl:and ,@body))
