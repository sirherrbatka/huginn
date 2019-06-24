(cl:in-package #:huginn.machine.representation)


(define-constant +cell-size+ (integer-length most-positive-fixnum))
(define-constant +tag-size+ 2)
(define-constant +word-size+ (- +cell-size+ +tag-size+))


(deftype cell ()
  `(unsigned-byte ,+cell-size+))


(deftype word ()
  `(unsigned-byte ,+word-size+))


(deftype tag ()
  `(unsigned-byte ,(1+ +tag-size+)))


(declaim (inline tag))
(defun tag (tag word)
  (declare (type word word)
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
           (optimize (speed 3)))
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
              (assert (typep (1- i) 'tag))
              (prog1 `(progn (define-constant ,tag-symbol ,i)
                             (declaim (inline ,predicate-symbol))
                             (defun ,predicate-symbol (cell)
                               (declare (type cell cell)
                                        (optimize (speed 3)))
                               (eql (tag-of cell) ,tag-symbol)))
                (incf i)))))
        tags)))


(define-tags
  +variable+ ; just a variable pointer (either bound or free depending on the variable-bindings)
  +reference+ ; variable pointing to expression. This tag simplifies unification algorithm implementation and allows filtering of clauses by scanining head alone. Dereferencing is more complicated then in the case of variable, first jump to the expression, next obtain word out of the expresion cell and use it as index for variable-bindings array.
  +fixnum+ ; integer small enough to fit in the word
  +expression+ ; complex expression, word in this cell designates arity, following cells are arguments to the expression.
  )


(defmacro tag-case ((cell) &body cases)
  (assert (every (rcurry #'member '(:variable :reference :expression :fixnum))
                 (mapcar #'first (plist-alist cases))))
  (with-gensyms (!tag)
    `(let ((,!tag (tag-of ,cell)))
       (cond
         ,@(~> (list
                (when-let ((form (getf cases :variable)))
                  `((eql ,!tag +variable+)
                    ,form))
                (when-let ((form (getf cases :reference)))
                  `((eql ,!tag +reference+)
                    ,form))
                (when-let ((form (getf cases :expression)))
                 `((eql ,!tag +expression+)
                   ,form))
                (when-let ((form (getf cases :fixnum)))
                  `((eql ,!tag +fixnum+)
                    ,form)))
               (remove-if #'null _))))))


(declaim (inline variable-or-reference-cell-p))
(defun variable-or-reference-cell-p (cell)
  (declare (type cell cell))
  (< (tag-of cell) 2))


(defun combine-tags (first-cell second-cell)
  (declare (type cell first-cell second-cell)
           (optimize (speed 3)))
  (logior (ash (the fixnum (1- (tag-of first-cell)))
               (1+ +tag-size+))
          (1- (tag-of second-cell))))


(flet ((combine-tags (first-tag second-tag)
         (combine-tags (tag first-tag 0)
                       (tag second-tag 0))))
  (define-constant +var-var+ (combine-tags +variable+ +variable+))
  (define-constant +var-ref+ (combine-tags +variable+ +reference+))
  (define-constant +var-exp+ (combine-tags +variable+ +expression+))
  (define-constant +var-fixnum+ (combine-tags +variable+ +fixnum+))
  (define-constant +ref-var+ (combine-tags +reference+ +variable+))
  (define-constant +ref-ref+ (combine-tags +reference+ +reference+))
  (define-constant +ref-exp+ (combine-tags +reference+ +expression+))
  (define-constant +ref-fixnum+ (combine-tags +reference+ +fixnum+))
  (define-constant +exp-var+ (combine-tags +expression+ +variable+))
  (define-constant +exp-ref+ (combine-tags +expression+ +reference+))
  (define-constant +exp-exp+ (combine-tags +expression+ +expression+))
  (define-constant +exp-fixnum+ (combine-tags +expression+ +fixnum+))
  (define-constant +fixnum-var+ (combine-tags +fixnum+ +variable+))
  (define-constant +fixnum-ref+ (combine-tags +fixnum+ +reference+))
  (define-constant +fixnum-exp+ (combine-tags +fixnum+ +expression+))
  (define-constant +fixnum-fixnum+ (combine-tags +fixnum+ +fixnum+)))


(declaim (inline variable-unbound-p))
(defun variable-unbound-p (cell)
  (declare (type cell cell)
           (optimize (speed 3)))
  (assert (eql +variable+ (tag-of cell)))
  (~> cell detag zerop))


(declaim (inline same-cells-p))
(defun same-cells-p (first-cell second-cell)
  (eql first-cell second-cell))


(declaim (inline make-reference))
(defun make-reference (pointer)
  (declare (type pointer pointer)
           (optimize (speed 3)))
  (tag +reference+ pointer))
