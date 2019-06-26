(cl:in-package #:huginn.compiler)


(defun anonymus-variable-p (variable)
  (and (symbolp variable)
       (string= (symbol-name variable) "?")))


(defun variable-p (variable)
  (and (symbolp variable)
       (char= #\? (aref (symbol-name variable) 0))))


(defun gather-all-sublists (list &key
                                   (index 0)
                                   (result (make-hash-table :test 'eq)))
  (labels ((impl (sublist)
             (when (consp sublist)
               (let ((new-index (ensure (gethash sublist result)
                                  index)))
                 (when (eql new-index index)
                   (incf index (+ 2 (length sublist))))
                 (map nil #'impl sublist)))))
    (impl list)
    result))


(defun gather-all-variable-bindings
    (list &key
            (index 0)
            (result (make-hash-table :test 'eq)))
  (~>> list flatten
       (remove-if (lambda (x)
                    (or (variable-p x)
                        (typep x 'huginn.m.r:word))))
       (map nil (lambda (variable)
                  (let ((new-index (ensure (gethash variable result)
                                     index)))
                    (when (eql new-index index)
                      (incf index))))))
  result)


(defclass compilation-state (fundamental-compilation-state)
  ((%forms-table :initarg :forms-table
                 :reader forms-table)
   (%content-size :initarg :content-size
                  :reader read-content-size
                  :reader cells-count)
   (%head :initarg :head
          :reader read-head)
   (%body :initarg :body
          :reader read-body))
  (:default-initargs
   :forms-table (make-hash-table :test 'eq)))
