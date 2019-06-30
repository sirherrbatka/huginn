(cl:in-package #:huginn.compiler)


(defclass fundamental-compilation-state ()
  ())

(defgeneric make-compilation-state (class content))

(defgeneric content (compilation-state))

(defgeneric pointer-for-variable (compilation-state variable))

(defgeneric pointer-for-expression (compilation-state expression))

(defgeneric expressions (compilation-state start end))

(defgeneric variable-bindings (compilation-state))

(defgeneric head (compilation-state))

(defgeneric body (compilation-state))

(defgeneric body-pointer (compilation-state))

(defgeneric predicate (compilation-state))

(defgeneric compile-clause (compilation-state))

(defgeneric variable-bindings (compilation-state))
