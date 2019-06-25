(cl:in-package #:huginn.compiler)


(defclass fundamental-compilation-state ()
  ())

(defgeneric cells-count (compilation-state))

(defgeneric make-compilation-state (class head body))

(defgeneric body-pointer (compilation-state))

(defgeneric content (compilation-state))

(defgeneric variable-bindings (compilation-state))

(defgeneric goal-pointers (compilation-state))
