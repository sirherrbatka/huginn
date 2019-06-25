(cl:in-package #:huginn.compiler)


(defun gather-all-sublists (list &key
                                   (index 0)
                                   (result (make-hash-table :test 'eq)))
  (labels ((impl (sublist)
             (when (consp sublist)
               (let ((new-index (ensure (gethash sublist result)
                                  index)))
                 (when (eql new-index index)
                   (incf index))
                 (map nil #'impl sublist)))))
    (impl list)
    result))


(defun gather-all-variables (list &key
                                    (index 0)
                                    (result (make-hash-table :test 'eq)))
  (~> list flatten
      (remove-if (lambda (x) (typep x 'huginn.m.r:word)) _)
      (map nil (lambda (variable)
                 (let ((new-index (ensure (gethash variable result)
                                    index)))
                   (when (eql new-index index)
                     (incf index))))))
  result)
