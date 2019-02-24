(in-package :aoc)

;;;; General ------------------------------------------------------------------

(defun summation (x &key (key 'identity))
  "Returns the sum of all the elements of `x`, or 0 if `x` is _empty_.

  If `key` is specified, this function will return the sum of all
  the values of `x`, `map`-ed using `key`."
  (loop
    :for e :being :the :elements :of x
    :for v = (funcall key e)
    :summing v))

(defun dividesp (divisor number)
  "Returns `T` if `DIVISOR` divies `NUMBER`."
  (zerop (rem number divisor)))

(defmacro aesthetic-string (data)
  `(format NIL "~A" ,data))

(defun sorted (s &optional (predicate 'char<))
  "Sort `s`, destructively"
  (sort s predicate))

(defun nsorted (s &optional (predicate 'char<))
  "Non-destructive version of `SORTED`"
  (sorted (copy-seq s) predicate))

(defun frequencies (s)
  "Returns a hash-table containing per each element of `s`, the number
  of times such element occurs in `s`"
  (loop :with freqs = (make-hash-table)
        :for c :across s
        :do (if (not (gethash c freqs))
              (setf (gethash c freqs) 1)
              (incf (gethash c freqs)))
        :finally (return freqs)))

(defun hamming-distance (s1 s2 &key (test 'eql))
  "Number of positional differences in two equal length strings."
  (when (= (length s1) (length s2))
    (count NIL (map 'list test s1 s2))))

(defun manhattan-distance (a b)
  "Calculate the manthattan distance between `a` and `b`.

  If `a` is a `NUMBER`, its real and imaginary parts will be used as X and Y values (same for `b`).

  Otherwise, `a` and `b` are treated as multi-dimensional sequencies."
  (if (numberp a)
    (+ (abs (- (realpart a)
               (realpart b)))
       (abs (- (imagpart a)
               (imagpart b))))
    (reduce #'+ (mapcar #'abs (mapcar #'- a b)))))

(defmacro curry (function &rest args)
  (let* ((i (position '>< args :test 'string=))
         (before (if i (subseq args 0 i) args))
         (after (and i (subseq args (1+ i)))))
    (with-gensyms (more-arg)
      `(lambda (,more-arg)
         (funcall ,function ,@before ,more-arg ,@after)))))

(defmacro recursively (bindings &body body)
  "Execute `body` recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) starting values.

  In `body` the symbol `recur` will be bound to the function for recurring."
  (let ((names (mapcar #'(lambda (b) (if (atom b) b (first b))) bindings))
        (values (mapcar #'(lambda (b) (if (atom b) nil (second b))) bindings)))
    `(labels ((recur (,@names)
                ,@body))
      (recur ,@values))))

(defmacro gathering (&body body)
  "Run `body` to gather some things and return a fresh list of them.

  `body` will be executed with the symbol `gather` bound to a
  function of one argument.  Once `body` has finished, a list of
  everything `gather` was called on will be returned.

  It's handy for pulling results out of code that executes
  procedurally and doesn't return anything, like `maphash` or
  Alexandria's `map-permutations`.

  The `gather` function can be passed to other functions, but should
  not be retained once the `gathering` form has returned (it would
  be useless to do so anyway).

  Examples:

    (gathering
      (dotimes (i 5)
        (gather i))
    =>
    (0 1 2 3 4)

    (gathering
      (mapc #'gather '(1 2 3))
      (mapc #'gather '(a b)))
    =>
    (1 2 3 a b)

  "
  (with-gensyms (result)
    `(let ((,result nil))
       (flet ((gather (item)
                (push item ,result)
                item))
         ,@body)
       (nreverse ,result))))

;;;; Hash keys ---------------------------------------------------------------

(defun hash-table-keys (h)
  "Return the hash keys of `h`"
  (loop
    :for k :being :the :hash-key :of h
    :collecting k))

(defun hash-table-values (h)
  "Return the hash values of `h`"
  (loop
    :for v :being :the :hash-value :of h
    :collecting v))

(defun hash-table-find (elem h &key (test 'eql))
  "Returns the first key in `h`, whose value is equal to `elem`"
  (loop
    :for key :being :the :hash-keys :of h
    :do (when (funcall test elem (gethash key h))
          (return key))))

(defun print-hash-table (h)
  (progn
    (dolist (k (hash-table-keys h))
      (format T "~a -> ~a~%" k (gethash k h)))
    (terpri)
    (finish-output)))

;;;; Disjoint-set (Union/find) ------------------------------------------------

(defstruct (disjointset (:constructor make-disjointset%)
                        (:copier nil))
  value
  rank
  parent)

(defun make-disjointset (value)
  (let* ((s (make-disjointset% :value value :rank 0)))
    (setf (disjointset-parent s) s)
    s))

(defun disjointset-find (x)
  (if (eq (disjointset-parent x) x)
    x
    (setf (disjointset-parent x) (disjointset-find (disjointset-parent x)))))

(defun disjointset-union (x y)
  (let ((x-root (disjointset-find x))
        (y-root (disjointset-find y)))
    (cond ((> (disjointset-rank x-root) (disjointset-rank y-root))
           (setf (disjointset-parent y-root) x-root))
          ((< (disjointset-rank x-root) (disjointset-rank y-root))
           (setf (disjointset-parent x-root) y-root))
          ((not (eq x-root y-root))
           (setf (disjointset-parent y-root) x-root)
           (incf (disjointset-rank x-root))))))

;;;; Doubly linked list -------------------------------------------------------

(defstruct dlink content prev next)

(defun dlink-removef (d)
  "Remove the `D` from the list, and return its `CONTENT`."
  (let* ((prev (dlink-prev d))
         (next (dlink-next d)))
    (setf (dlink-next prev) next
          (dlink-prev next) prev)
    (dlink-content d)))

(defun dlink-insertf (d content)
  "Insert `CONTENT` right after `D.

  Returns the newly inserted element.`"
  (let* ((next (dlink-next d))
         (new (make-dlink :content content)))
    (setf (dlink-next d) new
          (dlink-prev new) d
          (dlink-next new) next
          (dlink-prev next) new)))

;;;; Copy pasta ---------------------------------------------------------------

(defmacro with-gensyms (names &body body)
  "In fact, you've already seen one such pattern--many macros will, like the
  last version of do-primes, start with a LET that introduces a few variables
  holding gensymed symbols to be used in the macro's expansion. Since this is
  such a common pattern, why not abstract it away with its own macro?

  Borrowed from: http://www.gigamonkeys.com/book/macros-defining-your-own.html
  but tweaked (the signature)"
  `(let ,(loop :for n :in names :collecting `(,n (gensym)))
     ,@body))

(defun mkstr (&rest args)
  "Receives any number of objects (string, symbol, keyword, char, number),
  extracts all printed representations, and concatenates them all into one
  string.

  Extracted from _On Lisp_, chapter 4."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun mkstrc (&rest args)
  "Like `mkstr`, but concatenates with commas."
  (with-output-to-string (s)
    (format s "~{~a~^,~}" args)))

(defun symb (&rest args)
  "Receives any number of objects, concatenates all into one string with
  `#'mkstr` and converts them to symbol.

  Extracted from _On Lisp_, chapter 4.

  See also: `symbolicate`"
  (values (intern (apply #'mkstr args))))

(defun pr (&rest args)
  "Print `args` readably, separated by spaces and followed by a newline.
  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.
  This is what `print` should have been.

  https://github.com/sjl/cl-losh/blob/master/DOCUMENTATION.markdown#pr-macro
  "
  (format t "~{~S~^ ~}~%" args) ; Within the body segment, ~^ acts like pprint-exit-if-list-exhausted.
  (finish-output)
  (first args))

(defmacro prl (&rest args)
  "Print `args` labeled and readably.
  Each argument form will be printed, then evaluated and the result printed.
  One final newline will be printed after everything.
  Returns the last result.
  Examples:
    (let ((i 1)
          (l (list 1 2 3)))
      (prl i (second l)))
    ; =>
    i 1
    (second l) 2

  https://github.com/sjl/cl-losh/blob/master/DOCUMENTATION.markdown#prl-macro
  "
  `(prog1
     (progn ,@(mapcar (lambda (arg) `(pr ',arg ,arg)) args))
     (terpri)
     (finish-output)))

;;;; Iterators ----------------------------------------------------------------

(defmacro dorange ((var from to &optional ret) &body body)
  "Perform `body` on the given range of values.
  During iteration `body` will be executed with `var` bound to successive values
  in the range [`from`, `to`).
  Example:
    (dorange (x 5 8)
      (pr x y))
    ; =>
    5
    6
    7
  "
  `(loop :for ,var :from ,from :below ,to
         :do ,@body
         :finally (return ,ret)))

(defmacro doirange ((var from to &optional ret) &body body)
  "Similar to `DORANGE`, but `TO` is now included in the range."
  `(dorange (,var ,from (1+ ,to) ,ret)
     ,@body))

(defmacro dovector ((var vector &optional ret) &body body)
  "Perform `body` on all the elements of `vector`."
  `(loop :for ,var :across ,vector
         :do ,@body
         :finally (return ,ret)))

;;;; Streams ------------------------------------------------------------------
(defun read-all (file)
  "Reads the content of `file` and return a list of its lines."
  (loop
    :for i = (read-line file NIL :eof)
    :until (eq i :eof)
    :collecting i))

(defun parse-integers (x)
  "Parse a list of strings (representing integers) into a list of integers."
  (mapcar #'parse-integer x))

(defun read-integer (x)
  "Extract the first element from `X`, and parse is a integer."
  (first (parse-integers x)))

;;;; Problems -----------------------------------------------------------------
(defmacro define-problem ((year day)
                          (arg &optional (reader 'identity))
                          &body body)
  (multiple-value-bind (body declarations docstring)
      (alexandria:parse-body body :documentation t)
    (with-gensyms (file)
      (let ((run (symb 'problem-run)))
        `(defun ,run (&optional ,arg)
           ,@(when docstring (list docstring))
           ,@declarations
           (let ((,file (open (problem-input-path ,year ,day))))
             (unwind-protect
                 (progn (setf ,arg (,reader (read-all ,file)))
                        ,@body)
               (when ,file (close ,file)))))))))

(defun problem-input-path (year day)
  (make-pathname
    :directory `(:relative "." ,(aesthetic-string year))
    :name (format nil "day~2,'0D" day)
    :type "txt"))
