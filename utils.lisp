(in-package :aoc)

;;;; General ------------------------------------------------------------------
(defmacro summation (x)
  "Returns the sum of all the elements of `x`, or 0 if `x` is NIL"
  `(reduce #'+ ,x))

(defmacro aesthetic-string (data)
  `(format NIL "~A" ,data))

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

(defun hash-table-find (elem h &key (test 'eql))
  "Returns the first key in `h`, whose value is equal to `elem`"
  (loop
    :for key :being :the :hash-keys :of h
    :do (when (funcall test elem (gethash key h))
          (return key))))

(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

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

(defun symb (&rest args)
  "Receives any number of objects, concatenates all into one string with
  `#'mkstr` and converts them to symbol.

  Extracted from _On Lisp_, chapter 4.

  See also: `symbolicate`"
  (values (intern (apply #'mkstr args))))

(defun mkstr (&rest args)
  "Receives any number of objects (string, symbol, keyword, char, number),
  extracts all printed representations, and concatenates them all into one
  string.

  Extracted from _On Lisp_, chapter 4."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;;;; Streams ------------------------------------------------------------------
(defun read-all (file)
  (loop
    :for i = (read-line file NIL :eof)
    :until (eq i :eof)
    :collecting i))

(defun parse-integers (x)
  (mapcar #'parse-integer x))

(defun parse-list-of-chars (x)
  (coerce x 'list))

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
