(cl:in-package #:huginn.compiler)


(defun unique-index (&key (test 'eq) (start 0))
  (let ((table (make-hash-table :test test))
        (index start))
    (values
     (lambda (elt &rest all)
       (declare (ignore all))
       (let* ((fresh-index (ensure (gethash elt table) index))
              (new (eql fresh-index index)))
         (when new (incf index))
         (values fresh-index new)))
     table)))
