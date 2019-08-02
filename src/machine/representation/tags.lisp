(cl:in-package #:huginn.machine.representation)


(define-constant +cell-size+ (max (integer-length most-negative-fixnum)
                                  (integer-length most-positive-fixnum)))
(define-constant +tag-size+ 4)
(define-constant +word-size+ (- +cell-size+ +tag-size+))


(deftype cell ()
  `(unsigned-byte ,+cell-size+))


(deftype word ()
  `(unsigned-byte ,+word-size+))


(deftype tag ()
  `(integer 1 ,(ash 1 +tag-size+)))


(declaim (inline tag))
(defun tag (tag word)
  (declare (type (or cell word) word)
           (type tag tag))
  (dpb (1- tag) (byte +tag-size+ +word-size+) word))


(declaim (inline detag))
(defun detag (cell)
  (declare (type cell cell))
  (ldb (byte +word-size+ 0) cell))


(declaim (inline tag-of))
(-> tag-of (cell) tag)
(defun tag-of (cell)
  (declare (type cell cell)
           (optimize (speed 3) (safety 0) (debug 0)
                     (compilation-speed 0)))
  (1+ (ldb (byte +tag-size+ +word-size+) cell)))


(defmacro define-tags (&body tags)
  `(progn
     ,@(mapcar
        (let ((i 1))
          (lambda (tag-symbol)
            (let* ((predicate-name
                     (format nil "~a-CELL-P"
                             (remove-if (curry #'eql #\+)
                                        (symbol-name tag-symbol))))
                   (predicate-symbol (intern predicate-name)))
              (assert (typep i 'tag))
              (prog1 `(progn (define-constant ,tag-symbol ,i)
                             (declaim (inline ,predicate-symbol))
                             (defun ,predicate-symbol (cell)
                               (declare (optimize (speed 3) (safety 0) (space 0)
                                                  (debug 0) (compilation-speed 0))
                                        (type cell cell))
                               (eql (tag-of cell) ,tag-symbol)))
                (incf i)))))
        tags)))


(defmacro define-all-tags (&body input)
  `(progn
     (define-tags ,@input)
     (define-constant +all-tags+ (list ,@(mapcar (lambda (x) `(cons ',x ,x))
                                                 input))
       :test 'equal)))


(define-all-tags +variable+ +reference+ +fixnum+ +expression+
  +predicate+ +list-start+ +list-end+ +list-rest+)


(defun symbol-tag-of (cell)
  (declare (type cell cell))
  (car (find (tag-of cell) +all-tags+
             :key #'cdr)))


+all-tags+
(defmacro tag-case ((cell) &body cases)
  (assert (every (rcurry #'member '(:variable :reference :expression
                                    :fixnum :predicate :list-start
                                    :list-end :list-rest))
                 (mapcar #'first (plist-alist cases))))
  (with-gensyms (!tag)
    `(let ((,!tag (tag-of ,cell)))
       (cond
         ,@(~> (list
                (when-let ((form (getf cases :variable)))
                  `((eql ,!tag +variable+)
                    ,form))
                (when-let ((form (getf cases :list-start)))
                  `((eql ,!tag +list-start+)
                    ,form))
                (when-let ((form (getf cases :list-end)))
                  `((eql ,!tag +list-end+)
                    ,form))
                (when-let ((form (getf cases :list-rest)))
                  `((eql ,!tag +list-rest+)
                    ,form))
                (when-let ((form (getf cases :reference)))
                  `((eql ,!tag +reference+)
                    ,form))
                (when-let ((form (getf cases :expression)))
                 `((eql ,!tag +expression+)
                   ,form))
                (when-let ((form (getf cases :predicate)))
                  `((eql ,!tag +predicate+)
                    ,form))
                (when-let ((form (getf cases :fixnum)))
                  `((eql ,!tag +fixnum+)
                    ,form)))
               (remove-if #'null _))))))


(declaim (inline variable-or-reference-cell-p))
(defun variable-or-reference-cell-p (cell)
  (declare (type cell cell))
  (< (tag-of cell) 2))


(declaim (inline variable-unbound-p))
(defun variable-unbound-p (cell)
  (declare (type cell cell)
           (optimize (speed 3)))
  (assert (eql +variable+ (tag-of cell)))
  (~> cell detag zerop))


(declaim (inline predicate-unbound-p))
(defun predicate-unbound-p (cell)
  (declare (type cell cell)
           (optimize (speed 3)))
  (assert (eql +predicate+ (tag-of cell)))
  (~> cell detag zerop))


(declaim (inline same-cells-p))
(defun same-cells-p (first-cell second-cell)
  (eql first-cell second-cell))


(declaim (inline make-reference))
(defun make-reference (pointer)
  (declare (type pointer pointer)
           (optimize (speed 3)))
  (tag +reference+ pointer))


(declaim (inline list-rest-unbound-p))
(defun list-rest-unbound-p (cell)
  (declare (type cell cell)
           (optimize (speed 3)))
  (assert (eql +list-rest+ (tag-of cell)))
  (~> cell detag zerop))


(defun print-byte-code (vector stream &optional (end (length vector)))
  (macrolet ((s (c)
               `(format stream "[~a,~a:~a]" i ,c word)))
    (iterate
      (for i from 0 below end)
      (for cell = (aref vector i))
      (for word = (detag cell))
      (for previous-cell previous cell)
      (tag-case (cell)
        :fixnum (s #\f)
        :variable (s #\v)
        :list-start (s #\l)
        :list-rest (s #\m)
        :list-end (s #\t)
        :reference (s #\r)
        :expression (s #\e)
        :predicate (s #\p))))
  vector)
