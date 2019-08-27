(cl:in-package #:huginn.machine.operations)


(eval-always
  (defun find-body-for (cases tag1 tag2)
    (second (find (list (first tag1) (first tag2))
                  cases
                  :key #'first :test #'equal))))


(eval-always
  (defmacro cell-combination-case ((cell1 cell2) &body cases)
    (with-gensyms (!tag1 !tag2)
      `(let ((,!tag1 (huginn.m.r:tag-of ,cell1))
             (,!tag2 (huginn.m.r:tag-of ,cell2)))
         (declare (type huginn.m.r:tag ,!tag1 ,!tag2))
         (case ,!tag1
           ,@(iterate
               (for ptag in huginn.m.r:+all-tags+)
               (when (eq (car ptag) huginn.m.r:+reference+))
               (collect
                   `(,(cdr ptag)
                     (case ,!tag2
                       ,@(iterate
                           (for stag in huginn.m.r:+all-tags+)
                           (for body = (find-body-for cases ptag stag))
                           (unless (null body)
                             (collect (list (cdr stag) body)))))))))))))


(locally
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (space 0) (compilation-speed 0)))

  (-> prepare-unification-stack
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer)
      t)
  (defun prepare-unification-stack (execution-state stack-cell goal-pointer)
    (declare (type huginn.m.r:execution-stack-cell stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:pointer goal-pointer))
    (with-unification-stack (execution-state)
      (uclear)
      (upush (huginn.m.r:execution-stack-cell-heap-pointer stack-cell)
             goal-pointer)))


  (declaim (notinline alter-cell))
  (-> alter-cell
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:cell)
      huginn.m.r:cell)
  (defun alter-cell (execution-state
                     execution-stack-cell
                     pointer
                     new-value)
    (declare (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:pointer pointer)
             (type huginn.m.r:cell new-value))
    (let* ((heap-pointer (huginn.m.r:execution-stack-cell-heap-pointer
                          execution-stack-cell)))
      (unless (< pointer heap-pointer)
        ;; popping this stack-cell will unwind all the changes anyway, no need to worry with the undo stack
        (setf (aref (huginn.m.r:execution-state-heap execution-state)
                    pointer)
              new-value)
        (return-from alter-cell new-value))
      (let* ((trail #1=(huginn.m.r:execution-state-unwind-trail
                        execution-state))
             (trail-pointer
               #2=(huginn.m.r:execution-stack-cell-unwind-trail-pointer
                   execution-stack-cell))
             (length (length trail)))
        (incf #2# 2)
        (unless (< #2# length)
          (setf trail (adjust-array trail (the fixnum (ash length 2)))
                #1# trail))
        (setf (aref trail trail-pointer) pointer
              (aref trail (the fixnum (1+ trail-pointer)))
              (shiftf (aref (huginn.m.r:execution-state-heap execution-state)
                            pointer)
                      new-value))))
    new-value)


  (declaim (notinline unify-variable/list-start))
  (-> unify-variable/list-start
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-variable/list-start (execution-state
                                    execution-stack-cell
                                    variable-pointer
                                    list-start-pointer
                                    variable-cell
                                    list-start-cell)
    (declare (type huginn.m.r:cell variable-cell list-start-cell)
             (type huginn.m.r:pointer
                   list-start-pointer
                   variable-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (ignore list-start-pointer))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-variable/list-start nil))
    (alter-cell execution-state execution-stack-cell
                variable-pointer list-start-cell))



  (declaim (notinline unify-variable/list-start))
  (-> unify-list-rest/variable
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-list-rest/variable (execution-state
                                   execution-stack-cell
                                   list-rest-pointer
                                   variable-pointer
                                   list-rest-cell
                                   variable-cell)
    (declare (type huginn.m.r:cell variable-cell list-rest-cell)
             (type huginn.m.r:pointer
                   list-rest-pointer
                   variable-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-list-rest/variable nil))
    (alter-cell execution-state execution-stack-cell variable-pointer
                (if (huginn.m.r:list-rest-unbound-p list-rest-cell)
                    (huginn.m.r:make-reference list-rest-pointer)
                    (huginn.m.r:retag huginn.m.r:+list-start+
                                      list-rest-cell))))


  (declaim (notinline unify-list-end/list-rest))
  (-> unify-list-end/list-rest
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer
       huginn.m.r:pointer
       huginn.m.r:cell
       huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-list-end/list-rest (execution-state execution-stack-cell
                                   end-pointer rest-pointer
                                   end-cell rest-cell)
    (declare (ignore end-pointer))
    (unless (huginn.m.r:list-rest-unbound-p rest-cell)
      (return-from unify-list-end/list-rest nil))
    (alter-cell execution-state execution-stack-cell
                rest-pointer end-cell))


  (declaim (notinline unify-list-rests))
  (-> unify-list-rests
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer
       huginn.m.r:pointer
       huginn.m.r:cell
       huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-list-rests (execution-state execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2)
    (declare (type huginn.m.r:cell cell1 cell2)
             (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (cl-ds.utils:cond+ ((huginn.m.r:list-rest-unbound-p cell1)
                        (huginn.m.r:list-rest-unbound-p cell2))
      ((nil nil)
       (or (huginn.m.r:same-cells-p cell1 cell2)
           (unify-lists execution-state execution-stack-cell
                        (huginn.m.r:detag cell1)
                        (huginn.m.r:detag cell2))))
      ((t t)
       (when (> pointer2 pointer1)
         (rotatef pointer1 pointer2)
         (alter-cell execution-state execution-stack-cell
                     pointer1
                     (huginn.m.r:make-reference pointer2))))
      ((t nil)
       (alter-cell execution-state execution-stack-cell
                   pointer1 cell2))
      ((nil t)
       (alter-cell execution-state execution-stack-cell
                   pointer2 cell1))))


  (declaim (notinline unify-lists))
  (-> unify-lists
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer
       huginn.m.r:pointer)
      (or boolean huginn.m.r:cell))
  (defun unify-lists (execution-state execution-stack-cell
                      first-pointer second-pointer)
    (declare (type huginn.m.r:pointer first-pointer second-pointer)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell))
    (with-unification-stack (execution-state)
      (iterate
        (declare (type huginn.m.r:pointer p1 p2)
                 (type huginn.m.r:cell cell1 cell2))
        (with p1 = first-pointer)
        (with p2 = second-pointer)
        (when (= p1 p2)
          (done t))
        (for cell1 = (huginn.m.r:dereference-heap-pointer execution-state
                                                          p1 t))
        (for cell2 = (huginn.m.r:dereference-heap-pointer execution-state
                                                          p2 t))
        (for cell1-rest-p = (huginn.m.r:list-rest-cell-p cell1))
        (for cell2-rest-p = (huginn.m.r:list-rest-cell-p cell2))
        (cl-ds.utils:cond+ ((huginn.m.r:list-rest-cell-p cell1)
                            (huginn.m.r:list-rest-cell-p cell2))
          ((t t)
           (cl-ds.utils:cond+ ((huginn.m.r:list-rest-unbound-p cell1)
                               (huginn.m.r:list-rest-unbound-p cell2))
             ((nil nil)
              (setf p1 (huginn.m.r:detag cell1)
                    p2 (huginn.m.r:detag cell2))
              (next-iteration))
             ((nil t)
              (done (alter-cell execution-state execution-stack-cell
                                p2 cell1)))
             ((t nil)
              (done (alter-cell execution-state execution-stack-cell
                                p1 cell2)))
             ((t t)
              (when (< p2 p1) (rotatef p1 p2))
              (done (alter-cell execution-state execution-stack-cell
                                p2 (huginn.m.r:make-reference p1))))))
          ((nil t)
           (when (huginn.m.r:list-rest-unbound-p cell2)
             (done (alter-cell execution-state execution-stack-cell
                               p2 (huginn.m.r:tag huginn.m.r:+list-rest+ p1))))
           (setf p2 (huginn.m.r:follow-pointer execution-state
                                               (huginn.m.r:detag cell2)
                                               nil))
           (next-iteration))
          ((t nil)
           (when (huginn.m.r:list-rest-unbound-p cell1)
             (done (alter-cell execution-state execution-stack-cell
                               p1 (huginn.m.r:tag huginn.m.r:+list-rest+ p2))))
           (setf p1 (huginn.m.r:follow-pointer execution-state
                                               (huginn.m.r:detag cell1)
                                               nil))
           (next-iteration))
          ((nil nil) nil))
        (upush p1 p2)
        (incf p1)
        (incf p2)
        (until (or (huginn.m.r:list-end-cell-p cell1)
                   (huginn.m.r:list-end-cell-p cell2))))
      (done t)))


  (declaim (notinline unify-list-start/list-rest))
  (-> unify-list-start/list-rest
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-list-start/list-rest (execution-state
                                     execution-stack-cell
                                     list-start-pointer
                                     list-rest-pointer
                                     list-start-cell
                                     list-rest-cell)
    (declare (ignore list-start-pointer))
    (let ((list-start-word (huginn.m.r:detag list-start-cell))
          (list-rest-word (huginn.m.r:detag list-rest-cell)))
      (cond ((= list-start-word list-rest-word) t)
            ((huginn.m.r:list-rest-unbound-p list-rest-cell)
             (alter-cell execution-state
                         execution-stack-cell
                         list-rest-pointer
                         (huginn.m.r:tag huginn.m.r:+list-rest+
                                         list-start-word))
             t)
            (t (unify-lists execution-state
                            execution-stack-cell
                            list-start-word
                            list-rest-word)))))


  (declaim (notinline unify-list-rest/list-start))
  (-> unify-list-rest/list-start
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-list-rest/list-start (execution-state
                                     execution-stack-cell
                                     list-rest-pointer
                                     list-start-pointer
                                     list-rest-cell
                                     list-start-cell)
    (declare (type huginn.m.r:cell
                   list-start-cell list-rest-cell)
             (type huginn.m.r:pointer
                   list-rest-pointer list-start-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (ignore list-start-pointer))
    (when (huginn.m.r:list-rest-unbound-p list-rest-cell)
      (return-from unify-list-rest/list-start
        (alter-cell execution-state execution-stack-cell list-rest-pointer
                    (huginn.m.r:retag huginn.m.r:+list-rest+ list-start-cell))))
    (unify-lists execution-state execution-stack-cell
                 (huginn.m.r:detag list-start-cell)
                 (huginn.m.r:detag list-rest-cell)))


  (declaim (notinline unify-expressions))
  (-> unify-expressions
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer)
      boolean)
  (defun unify-expressions (execution-state
                            execution-stack-cell
                            first-pointer
                            second-pointer)
    (declare (type huginn.m.r:pointer
                   first-pointer
                   second-pointer)
             (ignore execution-stack-cell)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (or (= first-pointer second-pointer)
        (with-unification-stack (execution-state)
          (let* ((first-arity (huginn.m.r:detag (deref first-pointer)))
                 (second-arity (huginn.m.r:detag (deref second-pointer))))
            (declare (type fixnum first-arity second-arity))
            (unless (= first-arity second-arity)
              (done nil))
            (iterate
              (declare (type fixnum k j))
              (for j from 0 below first-arity)
              (for k from 1)
              (upush (the huginn.m.r:pointer
                          (+ first-pointer k))
                     (the huginn.m.r:pointer
                          (+ second-pointer k)))))
          (done t))))


  (declaim (notinline unify-variable/fixnum))
  (-> unify-variable/fixnum
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-variable/fixnum (execution-state
                                execution-stack-cell
                                variable-pointer
                                fixnum-pointer
                                variable-cell
                                fixnum-cell)
    (declare (ignore fixnum-pointer)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:pointer variable-pointer fixnum-pointer)
             (type huginn.m.r:cell fixnum-cell variable-cell))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-variable/fixnum nil))
    (alter-cell execution-state
                execution-stack-cell
                variable-pointer
                fixnum-cell))


  (declaim (notinline unify-variables))
  (-> unify-variables
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-variables (execution-state
                          execution-stack-cell
                          pointer1
                          pointer2
                          cell1
                          cell2)
    (declare (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:cell cell1 cell2))
    (assert (huginn.m.r:variable-cell-p cell1))
    (assert (huginn.m.r:variable-cell-p cell2))
    (cl-ds.utils:cond+ ((huginn.m.r:variable-unbound-p cell1)
                        (huginn.m.r:variable-unbound-p cell2))
      ((nil nil)
       (if (huginn.m.r:same-cells-p cell1 cell2) cell1 nil))
      ((t t)
       (when (< pointer2 pointer1)
         (rotatef pointer1 pointer2))
       (alter-cell execution-state execution-stack-cell
                   pointer2 (huginn.m.r:make-reference pointer1)))
      ((t nil)
       (alter-cell execution-state execution-stack-cell
                   pointer1 cell2))
      ((nil t)
       (alter-cell execution-state execution-stack-cell
                   pointer2 cell1))))


  (declaim (notinline unify-predicates))
  (-> unify-predicates
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-predicates (execution-state
                           execution-stack-cell
                           pointer1
                           pointer2
                           cell1
                           cell2)
    (declare (type huginn.m.r:pointer pointer1 pointer2))
    (cl-ds.utils:cond+ ((huginn.m.r:predicate-unbound-p cell1)
                        (huginn.m.r:predicate-unbound-p cell2))
      ((nil nil) (huginn.m.r:same-cells-p cell1 cell2))
      ((t t)
       (unless (> pointer1 pointer2)
         (rotatef pointer1 pointer2))
       (alter-cell execution-state execution-stack-cell
                   pointer1 (huginn.m.r:make-reference pointer2)))
      ((t nil) (alter-cell execution-state execution-stack-cell
                           pointer1 cell2))
      ((nil t) (alter-cell execution-state execution-stack-cell
                           pointer2 cell1))))


  (declaim (notinline unify-variable/expression))
  (-> unify-variable/expression
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      (or huginn.m.r:cell boolean))
  (defun unify-variable/expression (execution-state
                                    execution-stack-cell
                                    variable-pointer
                                    expression-pointer
                                    variable-cell
                                    expression-cell)
    (declare (ignore expression-pointer))
    (unless (huginn.m.r:variable-unbound-p variable-cell) ; can't change bound variable
      (return-from unify-variable/expression nil))
    (alter-cell execution-state
                execution-stack-cell
                variable-pointer
                expression-cell))


  (declaim (notinline unify-inner-list-rest))
  (-> unify-inner-list-rest
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer)
      (or huginn.m.r:cell boolean))
  (defun unify-inner-list-rest (execution-state
                                execution-stack-cell
                                list-rest-pointer
                                other-pointer)
    (declare (optimize (speed 0) (debug 3)))
    (let ((list-rest-cell (huginn.m.r:dereference-heap-pointer
                           execution-state
                           list-rest-pointer))
          (other-cell (huginn.m.r:dereference-heap-pointer
                       execution-state
                       other-pointer
                       t)))
      (cond ((huginn.m.r:list-rest-cell-p other-cell)
             (unify-list-rests execution-state execution-stack-cell
                               list-rest-pointer other-pointer
                               list-rest-cell other-cell))
            ((huginn.m.r:list-rest-unbound-p list-rest-cell)
             (alter-cell execution-state execution-stack-cell
                         list-rest-pointer
                         (huginn.m.r:tag huginn.m.r:+list-rest+
                                         other-pointer)))
            (t (unify-lists execution-state execution-stack-cell
                            (huginn.m.r:detag list-rest-cell)
                            other-pointer)))))


  (-> unify-pair
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       &optional
       (or null huginn.m.r:cell)
       (or null huginn.m.r:cell))
      (or huginn.m.r:cell boolean))
  (declaim (notinline unify-pair))
  (defun unify-pair (execution-state execution-stack-cell pointer1 pointer2
                     &optional cell1 cell2)
    (declare (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (when (eql pointer1 pointer2)
      (return-from unify-pair t))
    (bind ((cell1 (or cell1
                      (huginn.m.r:dereference-heap-pointer execution-state
                                                           pointer1)))
           (cell2 (or cell2
                      (huginn.m.r:dereference-heap-pointer execution-state
                                                           pointer2))))
      (declare (type huginn.m.r:cell cell1 cell2))
      (unless (< (huginn.m.r:tag-of cell1)
                 (huginn.m.r:tag-of cell2))
        (rotatef cell1 cell2)
        (rotatef pointer1 pointer2))
      (cell-combination-case (cell1 cell2)
        ((huginn.m.r:+variable+ huginn.m.r:+variable+)
         (unify-variables execution-state
                          execution-stack-cell
                          pointer1 pointer2
                          cell1 cell2))
        ((huginn.m.r:+variable+ huginn.m.r:+expression+)
         (unify-variable/expression execution-state execution-stack-cell
                                    pointer1 pointer2 cell1 cell2))
        ((huginn.m.r:+variable+ huginn.m.r:+fixnum+)
         (unify-variable/fixnum execution-state
                                execution-stack-cell
                                pointer1 pointer2
                                cell1 cell2))
        ((huginn.m.r:+variable+ huginn.m.r:+list-start+)
         (unify-variable/list-start execution-state
                                    execution-stack-cell
                                    pointer1 pointer2
                                    cell1 cell2))
        ((huginn.m.r:+variable+ huginn.m.r:+list-rest+)
         (unify-list-rest/variable execution-state
                                   execution-stack-cell
                                   pointer2 pointer1
                                   cell2 cell1))
        ((huginn.m.r:+predicate+ huginn.m.r:+predicate+)
         (unify-predicates execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2))
        ((huginn.m.r:+expression+ huginn.m.r:+expression+)
         (unify-expressions execution-state
                            execution-stack-cell
                            (huginn.m.r:detag cell1)
                            (huginn.m.r:detag cell2)))
        ((huginn.m.r:+fixnum+ huginn.m.r:+fixnum+)
         (huginn.m.r:same-cells-p cell1 cell2))
        ((huginn.m.r:+list-end+ huginn.m.r:+list-end+)
         cell1)
        ((huginn.m.r:+list-end+ huginn.m.r:+list-rest+)
         (unify-list-end/list-rest execution-state
                                   execution-stack-cell
                                   pointer1 pointer2
                                   cell1 cell2))
        ((huginn.m.r:+list-rest+ huginn.m.r:+list-rest+)
         (unify-list-rests execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2))
        ((huginn.m.r:+list-start+ huginn.m.r:+list-start+)
         (unify-lists execution-state execution-stack-cell
                      (huginn.m.r:detag cell1)
                      (huginn.m.r:detag cell2)))
        ((huginn.m.r:+list-start+ huginn.m.r:+list-rest+)
         (unify-list-start/list-rest execution-state
                                     execution-stack-cell
                                     pointer1 pointer2
                                     cell1 cell2)))))


  (defun unify-loop (execution-state execution-stack-cell)
    (declare (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (optimize (debug 3) (speed 0)))
    (with-unification-stack (execution-state)
      (when (uemptyp)
        (done t))
      (upop
       (first-pointer second-pointer)
       (let ((result
               (unify-pair execution-state execution-stack-cell
                           (huginn.m.r:follow-pointer execution-state
                                                      first-pointer t)
                           (huginn.m.r:follow-pointer execution-state
                                                      second-pointer t))))
         (unless result
           (done nil)))
       (next))))


  (declaim (notinline unify))
  (-> unify (huginn.m.r:execution-state
             huginn.m.r:execution-stack-cell
             huginn.m.r:pointer)
      (or huginn.m.r:cell boolean))
  (defun unify (execution-state execution-stack-cell goal-pointer)
    (declare (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:pointer goal-pointer))
    (let ((unify-head-function (~> execution-stack-cell
                                   huginn.m.r:execution-stack-cell-clause
                                   huginn.m.r:clause-unify-head-function)))
      (if (null unify-head-function)
          (progn
            (prepare-unification-stack execution-state
                                       execution-stack-cell
                                       goal-pointer)
            (unify-loop execution-state execution-stack-cell))
          (and (funcall unify-head-function
                        execution-state
                        execution-stack-cell
                        goal-pointer)
               (unify-loop execution-state execution-stack-cell))))))
