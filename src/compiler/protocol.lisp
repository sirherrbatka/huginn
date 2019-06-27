(cl:in-package #:huginn.compiler)


(defclass fundamental-compilation-state ()
  ())

(defgeneric cells-count (compilation-state))

(defgeneric make-compilation-state (class content))

(defgeneric body-pointer (compilation-state))

(defgeneric content (compilation-state))

(defgeneric pointer-for-variable (compilation-state variable))

(defgeneric pointer-for-expression (compilation-state expression))

(defgeneric variable-bindings (compilation-state))

(defgeneric variable-bindings-count (compilation-state))

(defgeneric variables (compilation-state))

(defgeneric expressions (compilation-state))

(defgeneric head (compilation-state))

(defgeneric body (compilation-state))

(defgeneric predicate (compilation-state))
