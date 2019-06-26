(cl:in-package #:huginn.compiler)


(define-constant +categories-list+ '(:expression :fixnum :reference :variable)
  :test 'equal)

(defun category-p (list)
  (and
    (listp list)
    (= (length (remove-duplicates list))
       (length list))
    (every (rcurry #'member +categories-list+)
           list)))

(deftype category ()
  `(satisfies category-p))

(defclass fundamental-compilation-state ()
  ())

(defgeneric cells-count (compilation-state))

(defgeneric make-compilation-state (class content))

(defgeneric body-pointer (compilation-state))

(defgeneric content (compilation-state))

(defgeneric pointer-for-variable (compilation-state variable))

(defgeneric variable-bindings (compilation-state))

(defgeneric pointers (compilation-state category))

(defgeneric variables (compilation-state))

(defgeneric expressions (compilation-state))
