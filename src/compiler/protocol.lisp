(cl:in-package #:huginn.compiler)


(defclass fundamental-compilation-state ()
  ())

(defgeneric make-compilation-state (class content))

(defgeneric cells-count (compilation-state))

(defgeneric content (compilation-state database))

(defgeneric pointer-for-variable (compilation-state variable))

(defgeneric pointer-for-expression (compilation-state expression))

(defgeneric pointer-for-predicate (compilation-state predicate))

(defgeneric expressions (compilation-state start end))

(defgeneric variables (compilation-state start end))

(defgeneric predicates (compilation-state start end))

(defgeneric variable-bindings (compilation-state))

(defgeneric head (compilation-state))

(defgeneric body (compilation-state))

(defgeneric body-pointer (compilation-state))

(defgeneric predicate (compilation-state))

(defgeneric variable-bindings (compilation-state))
