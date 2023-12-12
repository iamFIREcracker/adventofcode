;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:KEEP-IF :KEEP-IF-NOT :AAND :AIF :AWHEN :BND* :BND1 :COPY-ARRAY :COPY-HASH-TABLE :DIGITS :DIVF :DOLIST+ :DORANGE :DORANGEI :DOSEQ :DOSUBLISTS :ENUMERATE :FLATTEN :HASH-TABLE-ALIST :HASH-TABLE-KEY-EXISTS-P :HASH-TABLE-KEYS :HASH-TABLE-VALUES :IF-LET :IOTA :LOOPING :MAKE-KEYWORD :MKSTR :MULF :NCYCLE :REPEAT :STRING-ENDS-WITH-P :STRING-STARTS-WITH-P :SYMB :VOID :WHEN-LET :WHILE :WITH-GENSYMS) :ensure-package T :package "AOC.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "AOC.QUICKUTILS")
    (defpackage "AOC.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "AOC.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:ABBR :KEEP-IF :KEEP-IF-NOT :LET1 :AIF
                                         :AAND :AWHEN :BND* :BND1 :COPY-ARRAY
                                         :COPY-HASH-TABLE :DIGITS :DIVF
                                         :DOLIST+ :DORANGE :DORANGEI :DOSEQ
                                         :DOSUBLISTS :ENUMERATE :FLATTEN
                                         :HASH-TABLE-ALIST
                                         :HASH-TABLE-KEY-EXISTS-P :MAPHASH-KEYS
                                         :HASH-TABLE-KEYS :MAPHASH-VALUES
                                         :HASH-TABLE-VALUES :IF-LET :IOTA
                                         :MKSTR :SYMB :STRING-DESIGNATOR
                                         :WITH-GENSYMS :LOOPING :MAKE-KEYWORD
                                         :MULF :NCYCLE :REPEAT
                                         :STRING-ENDS-WITH-P
                                         :STRING-STARTS-WITH-P :VOID :WHEN-LET
                                         :WHILE))))

  (defmacro abbr (short long)
    "Defines a new function/macro named `short` and sharing
FDEFINITION/MACRO-FUNCTION with `long`."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (cond
         ((macro-function ',long)
          (setf (macro-function ',short) (macro-function ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         ((fboundp ',long)
          (setf (fdefinition ',short) (fdefinition ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         (t
           (error "Can't abbreviate ~a" ',long)))))
  
  (abbr keep-if remove-if-not)
  (abbr keep-if-not remove-if)

  (defmacro let1 (var val &body body)
    "Bind VAR to VAL within BODY. Equivalent to LET with one binding."
    `(let ((,var ,val))
       ,@body))
  

  (defmacro aif (test then &optional else)
    "Like IF, except binds the result of `test` to IT (via LET) for the scope of `then` and `else` expressions."
    (aif-expand test then else))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun aif-expand (test then &optional else)
      (let1 it (intern "IT")
        `(let1 ,it ,test
           (if ,it ,then ,else)))))
  

  (defmacro aand (&rest forms)
    "Like AND, except binds the result of each form to IT (via LET)."
    (aand-expand forms))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun aand-expand (forms)
      (cond ((not (car forms)) nil)
            ((not (cdr forms)) (car forms))
            (t (let1 car (car forms)
                 `(aif ,car
                    (aand ,@(cdr forms))))))))
  

  (defmacro awhen (test &body body)
    "Like WHEN, except binds the result of `test` to IT (via LET) for the scope of `body`."
    (awhen-expand test body))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun awhen-expand (test body)
      (let1 it (intern "IT")
        `(let1 ,it ,test
           (when ,it
             ,@body)))))
  

  (defmacro bnd* (bindings &body body)
    "Like LET*, but more powerful.

Use a symbol as the name of the binding to expand to a standard LET:

(bnd* (x
       (y (list 1 2 3)))
  (list x y)) ≡
(let (x)
  (let ((y (list 1 2 3)))
    (list x y)))

Use a list as the name of the binding to enable special type of expansions.

If the CAR of the list is the symbol VALUES, expand to MULTIPLE-VALUE-BIND
call:

(bnd* (((values f r) (floor 130 11)))
  (list f r)) ≡
(multiple-value-bind (f r)
     (floor 130 11)
   (list f r))

If the CAR of the list is the symbol WITH-SLOTS, expand to a WITH-SLOTS call:

(bnd* (((with-slots x y) thing))
  (incf x) (incf y))
≡
(with-slots (x y) thing
  (incf x) (incf y))

Otherwise, if the name of the binding is a list but none of the above applies,
BND* will expand to a DESTRUCTURING-BIND call:

(bnd* (((a b) '(1 2)))
  (list a b))
≡
(destructuring-bind (a b)
    '(1 2)
  (list a b))"
    (labels ((mklist (x) (if (atom x) (list x) x))
             (expand (bb)
               (cond ((null bb) (signal 'unexpected))
                     (t (let* ((b (mklist (car bb)))
                               (var (car b))
                               (val (cadr b)))
                          (cond ((symbolp var)
                                 `(let (,b)
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                ((eq (car var) 'values)
                                 `(multiple-value-bind ,(rest var) ,val
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                ((eq (car var) 'with-slots)
                                 `(with-slots ,(rest var) ,val
                                    ,@(if (rest bb)
                                        (list (expand (rest bb)))
                                        body)))
                                (t `(destructuring-bind ,@b
                                      ,@(if (rest bb)
                                          (list (expand (rest bb)))
                                          body)))))))))
      (expand bindings)))
  

  (defmacro bnd1 (binding &body body)
    "Equivalent to BND* with one binding."
    `(bnd* (,binding)
       ,@body))
  

  (defun copy-array (array &key (element-type (array-element-type array))
                                (fill-pointer (and (array-has-fill-pointer-p array)
                                                   (fill-pointer array)))
                                (adjustable (adjustable-array-p array)))
    "Returns an undisplaced copy of `array`, with same `fill-pointer` and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
    (let* ((dimensions (array-dimensions array))
           (new-array (make-array dimensions
                                  :element-type element-type
                                  :adjustable adjustable
                                  :fill-pointer fill-pointer)))
      (dotimes (i (array-total-size array))
        (setf (row-major-aref new-array i)
              (row-major-aref array i)))
      new-array))
  

  (defun copy-hash-table (table &key key test size
                                     rehash-size rehash-threshold)
    "Returns a copy of hash table `table`, with the same keys and values
as the `table`. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, `key`
is invoked on the value. As `key` defaults to `cl:identity`, a shallow
copy is returned by default."
    (setf key (or key 'identity))
    (setf test (or test (hash-table-test table)))
    (setf size (or size (hash-table-size table)))
    (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
    (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
    (let ((copy (make-hash-table :test test :size size
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold)))
      (maphash (lambda (k v)
                 (setf (gethash k copy) (funcall key v)))
               table)
      copy))
  

  (defun digits (n &optional (base 10))
    "Return a list of the digits of the non-negative integer `n` in base
`base`. By default, decimal digits are returned.

The order of the digits is such that the `k`th element of the list refers to the coefficient of `base^k`. In other words, given the resulting list

    (c0 c1 c2 ... ck)

the following identity holds:

    n = c0 + c1*base + c2*base^2 + ... + ck*base^k."
    (check-type n (integer 0))
    (check-type base (integer 2))
    (loop :with remainder
          :do (setf (values n remainder) (truncate n base))
          :collect remainder
          :until (zerop n)))
  

  (define-modify-macro divf (&optional (1/ratio 2)) /
    "A modifying version of division, similar to `decf`.")
  

  (defmacro dolist+ ((var list &optional (result nil result?)) &body body)
    "Like DOLIST, except it supports destructuring of `var`.

  > (let ((list '((1 a) (2 b))))
      (dolist+ ((a b) list :ret)
        (print (list a b))))
  ;;(1 A)
  ;;(2 B)
  :RET
  "
    `(loop :for ,var :in ,list do ,@body ,@(when result? `(:finally (return ,result)))))
  

  (defmacro dorange ((var from to &optional (step 1) (result nil result?)) &body body)
    "Binds `var` to all the distinct values in the range [`from`, `to`[, with
`step` step (note: `to` is excluded), and runs `body` inside that
lexical environmnet."
    (let ((step-g (gensym "step"))
          (to-g (gensym "to")))
      `(do* ((,step-g ,step)
             (,to-g ,to)
             (,var ,from (+ ,var ,step-g)))
         ((if (>= ,step-g 0) (>= ,var ,to-g) (<= ,var ,to-g))
          ,@(when result? `(,result)))
         ,@body)))
  

  (defmacro dorangei ((var from to &optional (step 1) (result nil result?)) &body body)
    "Like DORANGE, `to` is inclusive (the range is: [`from`, `to`])."
    (let ((step-g (gensym "step"))
          (to-g (gensym "to")))
      `(do* ((,step-g ,step)
             (,to-g ,to)
             (,var ,from (+ ,var ,step-g)))
         ((if (>= ,step-g 0) (> ,var ,to-g) (< ,var ,to-g))
          ,@(when result? `(,result)))
         ,@body)))
  

  (defmacro doseq ((var seq &optional return) &body body)
    "Iterate across the sequence `seq`, binding the variable `var` to
each element of the sequence and executing `body`. Return the value
`return` from the iteration form."
    `(block nil
       (map nil #'(lambda (,var)
                    (tagbody
                       ,@body))
            ,seq)
       ,return))
  

  (defmacro dosublists ((var list &optional (result nil result?)) &body body)
    "Like DOLIST, except:

- `var` is bound to successive sublists of `list` (similar to MAPL, LOOP..ON)
- `var` can lambda-list (similar to DOLIST+)
"
    `(loop :for ,var :on ,list do ,@body ,@(when result? `(:finally (return ,result)))))
  

  (defgeneric enumerate (x)
    (:documentation "Equivalent to `(zip (iota (length x)) x)`."))

  (defmethod enumerate ((x list))
    "Equivalent to `(zip (iota (length x)) x)`."
    (loop
      :for i :in x
      :for j :from 0
      :collect (list j i)))

  (defmethod enumerate ((x array))
    "Equivalent to `(zip (iota (length x)) x)`."
    (loop
      :for i :across x
      :for j :from 0
      :collect (list j i)))
  

  (defun flatten (&rest xs)
    "Flatten (and append) all lists `xs` completely."
    (labels ((rec (xs acc)
               (cond ((null xs)  acc)
                     ((consp xs) (rec (car xs) (rec (cdr xs) acc)))
                     (t          (cons xs acc)))))
      (rec xs nil)))
  

  (defun hash-table-alist (table)
    "Returns an association list containing the keys and values of hash table
`table`."
    (let ((alist nil))
      (maphash (lambda (k v)
                 (push (cons k v) alist))
               table)
      alist))
  

  (defun hash-table-key-exists-p (hash-table key)
    "Does `key` exist in `hash-table`?"
    (nth-value 1 (gethash key hash-table)))
  

  (declaim (inline maphash-keys))
  (defun maphash-keys (function table)
    "Like `maphash`, but calls `function` with each key in the hash table `table`."
    (maphash (lambda (k v)
               (declare (ignore v))
               (funcall function k))
             table))
  

  (defun hash-table-keys (table)
    "Returns a list containing the keys of hash table `table`."
    (let ((keys nil))
      (maphash-keys (lambda (k)
                      (push k keys))
                    table)
      keys))
  

  (declaim (inline maphash-values))
  (defun maphash-values (function table)
    "Like `maphash`, but calls `function` with each value in the hash table `table`."
    (maphash (lambda (k v)
               (declare (ignore k))
               (funcall function v))
             table))
  

  (defun hash-table-values (table)
    "Returns a list containing the values of hash table `table`."
    (let ((values nil))
      (maphash-values (lambda (v)
                        (push v values))
                      table)
      values))
  

  (defmacro if-let (bindings &body (then-form &optional else-form))
    "Creates new variable bindings, and conditionally executes either
`then-form` or `else-form`. `else-form` defaults to `nil`.

`bindings` must be either single binding of the form:

    (variable initial-form)

or a list of bindings of the form:

    ((variable-1 initial-form-1)
     (variable-2 initial-form-2)
     ...
     (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the `then-form` is executed with the
bindings in effect, otherwise the `else-form` is executed with the bindings in
effect."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (if (and ,@variables)
             ,then-form
             ,else-form))))
  

  (declaim (inline iota))
  (defun iota (n &key (start 0) (step 1))
    "Return a list of `n` numbers, starting from `start` (with numeric contagion
from `step` applied), each consequtive number being the sum of the previous one
and `step`. `start` defaults to `0` and `step` to `1`.

Examples:

    (iota 4)                      => (0 1 2 3)
    (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
    (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)"
    (declare (type (integer 0) n) (number start step))
    (loop repeat n
          ;; KLUDGE: get numeric contagion right for the first element too
          for i = (+ (- (+ start step) step)) then (+ i step)
          collect i))
  

  (defun mkstr (&rest args)
    "Receives any number of objects (string, symbol, keyword, char, number), extracts all printed representations, and concatenates them all into one string.

Extracted from _On Lisp_, chapter 4."
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  

  (defun symb (&rest args)
    "Receives any number of objects, concatenates all into one string with `#'mkstr` and converts them to symbol.

Extracted from _On Lisp_, chapter 4.

See also: `symbolicate`"
    (values (intern (apply #'mkstr args))))
  

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  

  (defmacro looping (&body body)
    "Run `body` in an environment where the symbols COLLECT!, APPEND!, SUM!,
COUNT!, MINIMIZE!, and MAXIMIZE! are bound to functions that can be used to
collect / append, sum, count, minimize or maximize things respectively.

Mixed usage of COLLECT!/APPEND!, SUM!, COUNT!, MINIMIZE! and MAXIMIZE! is not
supported.

Examples:

  (looping
    (dotimes (i 5)
      (if (oddp i)
        (collect! i))))
  =>
  (1 3)

  (looping
    (dotimes (i 5)
      (if (oddp i)
        (sum! i))))
  =>
  4

  (looping
    (dotimes (i 5)
      (count! (oddp i))))
  =>
  2

  (looping
    (dotimes (i 5)
      (sum! i)
      (count! (oddp i))))
  ;; Signals an ERROR: Cannot use COUNT! together with SUM!
  "
    (with-gensyms (loop-type result last collect-last)
      (labels ((extract-loop-type (body)
                 (cond ((null body) nil)
                       ((symbolp body) (find body
                                             '(collect! append! sum! count! minimize! maximize!)
                                             :test #'string=))
                       ((consp body) (or (extract-loop-type (car body))
                                         (extract-loop-type (cdr body))))))
               (init-result (loop-type)
                 (ecase loop-type
                   ((collect! append! minimize! maximixe!) nil)
                   ((sum! count!) 0))))
        (let* ((loop-type-value (extract-loop-type body))
               (result-value (init-result loop-type-value)))
          `(let* ((,loop-type ',loop-type-value)
                  (,result ,result-value)
                  (,last nil))
             (declare (ignorable ,last))
             (labels ((,collect-last (item)
                       (if (not ,last)
                         (prog1 (push item ,result)
                           (setf ,last ,result))
                         (prog1 (push item (cdr ,last))
                           (setf ,last (cdr ,last)))))
                      (,(symb "COLLECT!") (item)
                       (if (and ,loop-type (and (not (eql ,loop-type 'collect!))
                                                (not (eql ,loop-type 'append!)) ))
                         (error "Cannot use COLLECT! together with ~A" ,loop-type)
                         (,collect-last item)))
                      (,(symb "APPEND!") (item)
                       (if (and ,loop-type (and (not (eql ,loop-type 'collect!))
                                                (not (eql ,loop-type 'append!)) ))
                         (error "Cannot use APPEND! together with ~A" ,loop-type)
                         (progn
                           (setf ,result (append ,result item)
                                 ,last (last item))
                           item)))
                      (,(symb "SUM!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'sum!)))
                         (error "Cannot use SUM! together with ~A" ,loop-type)
                         (progn
                           (incf ,result item)
                           item)))
                      (,(symb "COUNT!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'count!)))
                         (error "Cannot use COUNT! together with ~A" ,loop-type)
                         (progn
                           (when item
                             (incf ,result)
                             item))))
                      (,(symb "MINIMIZE!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'minimize!)))
                         (error "Cannot use MINIMIZE1 together with ~A" ,loop-type)
                         (setf ,result (min (or ,result item) item))))
                      (,(symb "MAXIMIZE!") (item)
                       (if (and ,loop-type (not (eql ,loop-type 'maximize!)))
                         (error "Cannot use MAXIMIZE! together with ~A" ,loop-type)
                         (setf ,result (max (or ,result item) item)))))
               ,@body)
             ,result)))))
  

  (defun make-keyword (name)
    "Interns the string designated by `name` in the `keyword` package."
    (intern (string name) :keyword))
  

  (define-modify-macro mulf (&optional (ratio 2)) *
    "A modifying version of multiplication, similar to `incf`.")
  

  (defun ncycle (list)
    "Mutate `list` into a circlular list."
    (nconc list list))
  

  (defmacro repeat (n &body body)
    "Runs BODY N times."
    `(loop repeat ,n do ,@body))
  

  (defun string-ends-with-p (suffix s)
    "Returns T if the last few characters of `s` are equal to `suffix`."
    (and (<= (length suffix) (length s))
         (string= suffix s :start2 (- (length s) (length suffix)))))
  

  (defun string-starts-with-p (prefix s)
    "Returns T if the first few characters of `s` are equal to `prefix`."
    (and (<= (length prefix) (length s))
         (string= prefix s :end2 (length prefix))))
  

  (defun void (&rest args)
    "Do absolutely nothing, and return absolutely nothing."
    (declare (ignore args))
    (values))
  

  (defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (when (and ,@variables)
           ,@forms))))

  (defmacro when-let* (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the WHEN-LET*.

Execution of WHEN-LET* stops immediately if any initial-form evaluates to NIL.
If all initial-forms evaluate to true, then FORMS are executed as an implicit
PROGN."
    (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                            (list bindings)
                            bindings)))
      (labels ((bind (bindings forms)
                 (if bindings
                     `((let (,(car bindings))
                         (when ,(caar bindings)
                           ,@(bind (cdr bindings) forms))))
                     forms)))
        `(let (,(car binding-list))
           (when ,(caar binding-list)
             ,@(bind (cdr binding-list) forms))))))
  

  (defmacro while (expression &body body)
    "Executes `body` while `expression` is true."
    `(loop while ,expression do
       ,@body))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(keep-if keep-if-not aand aif awhen bnd* bnd1 copy-array
            copy-hash-table digits divf dolist+ dorange dorangei doseq
            dosublists enumerate flatten hash-table-alist
            hash-table-key-exists-p hash-table-keys hash-table-values if-let
            iota looping make-keyword mkstr mulf ncycle repeat
            string-ends-with-p string-starts-with-p symb void when-let
            when-let* while with-gensyms with-unique-names)))

;;;; END OF quickutils.lisp ;;;;
