(cl:in-package #:huginn.compiler)


(defun gather-all-sublists
    (list &aux (index 0) (result (make-hash-table :test 'eq)))
  (labels ((impl (sublist)
             (when (consp sublist)
               (let ((new-index (ensure (gethash sublist result)
                                  index)))
                 (when (eql new-index index)
                   (incf index))
                 (map nil #'impl sublist)))))
    (impl list)
    result))
