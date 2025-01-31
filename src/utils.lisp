(in-package :aoc)

;;;; General ------------------------------------------------------------------

(defun array-elements (array)
  "Returns a list of all the elements of the array `array`, in row major order.

  (array-elements #2a((1 2) (3 4)))
  =>
  (1 2 3 4)

  (array-elements #3a(((1 2) (3 4)) ((5 6) (7 8))))
  =>
  (1 2 3 4 5 6 7 8)
  "
  (let ((index -1))
    (labels ((recur (dimensions)
               (cond ((not dimensions) (list (row-major-aref array (incf index))))
                     (t (loop repeat (car dimensions)
                              append (recur (cdr dimensions)))))))
      (recur (array-dimensions array)))))

(defun find-min (x &key (key 'identity) (predicate '<))
  "Finds the minimum element of `x`, or NIL if `x` is empty.

  By default this function assumes `x` to be a bag of numbers, but this
  can be overridden by providing a different value of `predicate`;
  `predicate` should accept two arguments and return non-NIL if
  `arg1` is _smaller_ than `arg2`.

  If `key` is specified, this function will return the element that
  minimizes `(funcall key elem)`; otherwise, calling this function will be
  the same as calling: `(reduce #'min x)`.

  The minimum value of `(funcall key elem)` is also returned as second value.

  (find-min '(4 3 2 1))
  =>
  1
  1

  (find-min '(#\d #\c #\b #\a) :predicate #'char<)
  =>
  #\a
  #\a

  (find-min '(4 3 2 1) :key (lambda (x) (- 4 x)))
  =>
  4
  0
  "
  (values-list
    (reduce
      #'(lambda (acc each &aux (value (funcall key each)))
         (if (or (not acc) (funcall predicate value (second acc)))
           (list each value)
           acc))
      x :initial-value nil)))

(defun find-max (x &key (predicate '>) (key 'identity))
  "Finds the maximum element of `x`, or NIL if `x` is empty.

  By default this function assumes `x` to be a bag of numbers, but this
  can be overridden by providing a different value of `predicate`;
  `predicate` should accept two arguments and return non-NIL if
  `arg1` is _greater_ than `arg2`.

  If `key` is specified, this function will return the element that
  maximises `(funcall key elem)`; otherwise, calling this function will be
  the same as calling: `(reduce #'max x)`.

  The maximum value of `(funcall key elem)` is also returned as second value.

  (find-max '(1 2 3 4))
  =>
  4
  4

  (find-max '(#\a #\b #\c #\d) :predicate #'char>)
  =>
  #\d
  #\d

  (find-max '(1 2 3 4) :key (lambda (x) (- 4 x)))
  =>
  1
  3
  "
  (if (not x)
    (values 0)
    (values-list
      (reduce
        #'(lambda (acc each &aux (value (funcall key each)))
           (if (or (not acc) (funcall predicate value (second acc)))
             (list each value)
             acc))
        x :initial-value nil))))

(defun dividesp (divisor number)
  "Returns `T` if `DIVISOR` divies `NUMBER`."
  (zerop (rem number divisor)))

(defmacro hexadecimal-string (seq)
  `(format NIL "~{~(~2,'0x~)~}" ,seq))

(defun hexadecimal->binary (s)
  (let* ((len (* 4 (length s)))
         (control-str (mkstr "~" len ",'0b"))
         (num (parse-integer s :radix 16)))
    (format NIL control-str num)))

(defun make-counter (x &key (test 'eql))
  "Returns a HASH-TABLE mapping _unique_ elements of `x` to the number
  of times they occur in `x`."
  (prog1-let map (make-hash-table :test test)
    (map nil (lambda (e) (incf (gethash e map 0))) x)))


(defun frequencies (x &key (test 'eql))
  "Returns an association list mapping _unique_ elements of `x` to the number
  of times they occur in `x`."
  (let (alist)
    (maphash (lambda (k v) (push (cons k v) alist)) (make-counter x :test test))
    alist))

(defun hamming-distance (s1 s2 &key (test 'eql))
  "Number of positional differences in two equal length strings."
  (when (= (length s1) (length s2))
    (count NIL (map 'list test s1 s2))))

(defun manhattan-distance (a b)
  "Calculate the manthattan distance between `a` and `b`.

  If `a` is a NUMBER, its real and imaginary parts will be used as X and Y values (same for `b`).

  Otherwise, `a` and `b` are treated as multi-dimensional sequencies."
  (cond ((numberp a) (+ (abs (- (realpart a)
                                (realpart b)))
                        (abs (- (imagpart a)
                                (imagpart b)))))
        ((not (third a)) (+ (abs (- (car a)
                                    (car b)))
                            (abs (- (cadr a)
                                    (cadr b)))))
        (t (reduce #'+ (mapcar #'abs (mapcar #'- a b))))))

;;; https://stackoverflow.com/a/8448611/348524
(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))


(defun as-keyword (string)
  "Converts `string` into a keyword symbol"
  (make-keyword (string-upcase string)))


(defun split (seq pos)
  "Partitions 'seq' at index 'pos'."
  (check-type pos (integer 0 *))
  (if (> (length seq) pos)
    (list (subseq seq 0 pos) (subseq seq pos))
    (list seq '())))


;;;; Control flow -------------------------------------------------------------


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-contain-placeholder-p (args placeholder)
    (recursively ((args args))
      (if (atom args)
        (eq args placeholder)
        (or (recur (car args))
            (recur (cdr args)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-replace (placeholder args name)
    (subst name placeholder args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-append (args name)
    (append args (list name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-replace-placeholder-or-append (args placeholder name)
    (if (args-contain-placeholder-p args placeholder)
      (args-replace placeholder args name)
      (args-append args name))))

(defmacro ->< (x &rest forms)
  "Threads the expr through the forms, like Clojure's `->`.

  While threadingg, for each element of `FORMS`:

  - if a SYMBOL, it's converted into a LIST and the accumulated value is appended to it
  - if a LIST already, the accumulated value is appended to it unless the list contains
  the placeholder `><` (in which case `><` is replaced with the accumulated value)

  Examples:
  (->< 'World
    (list 'Hello))
  =>
  (HELLO WORLD)

  (->< 'World
    (list >< 'Hello))
  =>
  (WORLD HELLO)

  (->< 'World
    (list >< 'Hello)
    reverse)
  =>
  (HELLO WORLD)
  "
  (with-gensyms (result)
    `(let* ((,result ,x)
            ,@(mapcar (lambda (form)
                        (if (symbolp form)
                          `(,result (,form ,result))
                          `(,result ,(args-replace-placeholder-or-append form
                                                                          '><
                                                                          result))))
                      forms))
        ,result)))

(defmacro with-slots-as-list (slot-names instance-form)
  "Extracts the values of the slots named `slot-names` from `instance-form`,
  and return them as a LIST.

  This might come in handy when using structs inside loops, and help you
  replace:

    :for foo = (foo instance)
    :for bar = (bar instance)

  with:

    :for (foo bar) = (with-slots-as-list (foo bar) instance)

  There is still some duplications unfortunately, but I like it more than the
  multiple `:for name = (slot-fn instance)` approach."
  `(with-slots ,slot-names ,instance-form
       (list ,@slot-names)))

;;;; Functional ---------------------------------------------------------------

(defmacro partial-1 (fn &rest args)
  "Returns a function that invokes `fn` with `args` prepended to the argument it
  receives.  The symbol _ may be used as a placeholder for where the received
  argument should be placed in the argument list.

  Example:
  (defun greet (greeting name)
    (list greeting name))

  (funcall (partial-1 #'greet 'hello) 'fred)
  ; =>
  (HELLO FRED)

  (funcall (partial-1 #'greet _ 'fred) 'hi)
  ; =>
  (HI FRED)
  "
  (with-gensyms (more-arg)
    (let ((actual-args (args-replace-placeholder-or-append args '_ more-arg)))
      `(lambda (,more-arg)
          (funcall ,fn ,@actual-args)))))

(defmacro partial-2 (fn &rest args)
  "Similar to PARTIAL-1, but the returned function eccepts two arguments instead
  of one.  Also, the symbols _1 and _2 can be used to place the arguments in the
  final argument list."
  (with-gensyms (more-arg moar-arg)
    (let* ((actual-args args)
           (actual-args (args-replace-placeholder-or-append actual-args '_1 more-arg))
           (actual-args (args-replace-placeholder-or-append actual-args '_2 moar-arg)))
      `(lambda (,more-arg ,moar-arg)
          (funcall ,fn ,@actual-args)))))

;;;; Memoization --------------------------------------------------------------

(defmacro defun/memo (name args &body body)
  "Defines a function (named `name`, with `args` as arguments, and `body` as body)
  whose return values are cached so that when invoked a second time with the same
  arguments, the cache will be used.

  It also defines another function (named `name`/clear-memo) to use to clear
  up the internal cache of results.

  Example:

    (defun fib(n)
      (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

    (time (fib 40))
    Evaluation took:
      3.513 seconds of real time
      3.504912 seconds of total run time (3.490136 user, 0.014776 system)
      99.77% CPU
      8,078,115,737 processor cycles
      0 bytes consed

    102334155

    (defun/memo fib(n)
      (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

    (time (fib 40))
    Evaluation took:
      0.000 seconds of real time
      0.000019 seconds of total run time (0.000018 user, 0.000001 system)
      100.00% CPU
      39,090 processor cycles
      0 bytes consed

    102334155
  "
  (expand-defun/memo name args body))

(defun expand-defun/memo (name args body)
  (let* ((memo (gensym  (mkstr name '-memo))))
    `(let ((,memo (make-hash-table :test 'equalp)))
      (values
        ,(expand-defun/memo-memoized-fun memo name args body)
        ,(expand-defun/memo-clear-memo-fun memo name)))))


(defun expand-defun/memo-memoized-fun (memo name args body)
  (with-gensyms (key result result-exists-p)
    `(defun ,name ,args
      (let ((,key (list ,@(expand-defun/memo-keyable-args args))))
        (multiple-value-bind (,result ,result-exists-p) (gethash ,key ,memo)
          (if ,result-exists-p
            ,result
            (setf (gethash ,key ,memo)
                  ,@body)))))))

(defun expand-defun/memo-keyable-args (args)
  (looping
    (loop while args for a = (pop args) do
          (cond ((eq a '&optional) nil)
                ((eq a '&key) nil)
                ((eq a '&rest) (pop args))
                ((eq a '&aux) (return))
                ((consp a) (collect! (car a)))
                (t (collect! a))))))


(defun expand-defun/memo-clear-memo-fun (memo name)
  (let ((clear-memo-name (intern (mkstr name '/clear-memo))))
    `(defun ,clear-memo-name ()
      (clrhash ,memo))))

(defmacro memoizing ((ht &rest key-parts) &body body)
  (with-gensyms (memo key)
    `(let ((,memo ,ht)
           (,key (list ,@key-parts)))
       (multiple-value-bind (res res?) (gethash ,key ,memo)
         (if res?
           res
           (setf (gethash ,key ,memo)
                 (block memo
                        ,@body)))))))

(defmacro unless-already-seen ((ht &rest key-parts) &body body)
  (with-gensyms (memo key)
    `(let ((,memo ,ht)
           (,key (list ,@key-parts)))
       (flet ((,(symb "UNSEE") () (remhash ,key ,memo)))
         (unless (gethash ,key ,memo)
           (setf (gethash ,key ,memo) t)
           (block unless-already-seen
                  ,@body))))))

;;;; Math ---------------------------------------------------------------------

(defmacro with-complex-parts ((real img) c &body body)
  (with-gensyms (complex)
    `(let* ((,complex ,c)
            (,real (realpart ,complex))
            (,img (imagpart ,complex)))
        ,@body)))

(defun complex-rotate-cw (c)
  "Rotate `c`, cloclwise."
  (complex (imagpart c) (- (realpart c))))

(defun complex-rotate-ccw (c)
  "Rotate `c`, counter-clockwise."
  (complex (- (imagpart c)) (realpart c)))

(defun <=> (n m)
  "Three-way comparison operator, a.k.a. spaceship operator.

  Returns:

  -1 when n < m
  0 when n = m
  1 when n > m"
  (signum (- n m)))

;;;; Hash tables --------------------------------------------------------------

(defun hash-table-insert (ht key value) ;; XXX this cannot be defined as macro, somehow..
  (setf (gethash key ht) value))

(defun print-hash-table (h)
  (progn
    (dolist (k (hash-table-keys h))
      (format T "~a -> ~a~%" k (gethash k h)))
    (terpri)
    (finish-output)))

(defun print-map-char (val &optional key)
  (declare (ignore key))
  val)

(defun print-hash-table-map (h &optional (key #'print-map-char) (stream T))
  (let ((min-x (reduce #'min (hash-table-keys h) :key #'realpart))
        (max-x (reduce #'max (hash-table-keys h) :key #'realpart))
        (min-y (reduce #'min (hash-table-keys h) :key #'imagpart))
        (max-y (reduce #'max (hash-table-keys h) :key #'imagpart)))
    (dorangei (y max-y min-y -1)
      (dorangei (x min-x max-x)
        (let ((pos (complex x y)))
          (format stream "~a" (funcall key (gethash pos h) pos))))
      (format stream "~&"))))

;;;; Disjoint-set (Union/find) ------------------------------------------------

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

;;;; Ring ---------------------------------------------------------------------

(defstruct (ring (:constructor make-ring%))
  current)

(defun make-ring (content)
  "Creates a circular `DLINK` having a single element with `CONTENT`"
  (let ((element (make-dlink :content content)))
    (setf (dlink-prev element) element
          (dlink-next element) element)
    (make-ring% :current element)))

(defun ring-movef (r offset)
  "Moves `r`'s `current` pointer `offset` elements forward (or backward)"
  (let ((move (if (> offset 0) #'dlink-next #'dlink-prev)))
    (dotimes (i (abs offset))
      (setf (ring-current r) (funcall move (ring-current r))))))

(defun ring-removef (r &aux (current (ring-current r)))
  "Removes `r`'s `current`"
  (let* ((content (dlink-removef current)))
    (setf (ring-current r) (dlink-next current))
    content))

(defun ring-insertf (r content &aux (current (ring-current r)))
  "Insert `content` right after `r`'s `current`"
  (setf (ring-current r) (dlink-insertf current content)))

;;;; Heap Queue ---------------------------------------------------------------

(defun make-hq (&key (predicate #'<) (key #'identity))
  "Creates an heap queue."
  (pileup:make-heap predicate :key key))

(defun hq-empty-p (hq)
  "Returns true if the heap is empty."
  (pileup:heap-empty-p hq))

(defun hq-pop (hq)
  "Pops the first element of the queue"
  (pileup:heap-pop hq))

(defun hq-insert (hq item)
  "Adds `item` to the queue."
  (pileup:heap-insert item hq))

;;;; Deque --------------------------------------------------------------------

;;;https://rosettacode.org/wiki/FIFO#Common_Lisp
(defstruct (queue (:constructor %make-queue))
  (items '() :type list)
  (tail '() :type list))

(defun make-queue ()
  "Returns an empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Returns true if the queue is empty."
  (endp (queue-items queue)))

(defun enqueue (item queue)
  "Enqueue item in queue. Returns the queue."
  (prog1 queue
    (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))))

(defun dequeue (queue)
  "Dequeues an item from queue. Signals an error if queue is empty."
  (if (queue-empty-p queue)
    (error "Cannot dequeue from empty queue.")
    (pop (queue-items queue))))

;;;; Summed-area Table (or Integral image) ------------------------------------

(defun aref-or (default array &rest subscripts)
  "Similar to AREF, but `DEFAULT` is returned if `SUBSCRIPTS` are not valid
  indices of `ARRAY`."
  (if (apply #'array-in-bounds-p array subscripts)
    (apply #'aref array subscripts)
    default))

(defun make-summedarea-table (grid &aux (st (make-array (array-dimensions grid))))
  "Creates a Summed-area Table.

  For additional information, visit:
  https://en.wikipedia.org/wiki/Summed-area_table.

  Example:

    (defvar *grid* (make-array '(2 2) :initial-contents '((0 1)
                                                          (2 3))))
    =>
    *GRID*

    (defparameter *st* (make-summedarea-table *original*))
    =>
    *ST*

    *st*
    =>
    #2A((0 1 3) (3 8 15) (9 21 36))
  "
  (dotimes (y (array-dimension st 0))
    (dotimes (x (array-dimension st 1))
      (setf (aref st y x)
            (->< (aref grid y x)
              (+ >< (aref-or 0 st (1- y) x))
              (+ >< (aref-or 0 st y (1- x)))
              (- >< (aref-or 0 st (1- y) (1- x)))))))
  st)

(defun st-area-of (st top left bottom right)
  "Calculates the sum of the values contained in the rectangle delimited by
  `TOP`, `LEFT`, `BOTTOM`, and `RIGHT`."
  (->< (aref-or 0 st bottom right)
    (- >< (aref-or 0 st (1- top) right))
    (- >< (aref-or 0 st bottom (1- left)))
    (+ >< (aref-or 0 st (1- top) (1- left)))))

;;;; Search -------------------------------------------------------------------

(defgeneric adjacents (pos &key include-diagonal)
  (:documentation
   "Return all the adjacents positions of POS.

    By default, only the 4 immediate adjacents positions are returned
    (i.e. top, right, bottom, and left), but INCLUDE-DIAGONAL can be used to
    also return diagonal positions too."))


(defparameter *nhood-d1-complex* '(#C(0 1) #C(1 0) #C(0 -1) #C(-1 0)))
(defparameter *nhood-diagonal-complex* (concatenate 'list
                                                    *nhood-d1-complex*
                                                    '(#C(1 1) #C(1 -1) #C(-1 -1) #C(-1 1))))
(defmethod adjacents ((pos number)
                      &key include-diagonal
                      &aux (deltas *nhood-d1-complex*))
  (when include-diagonal
    (setf deltas *nhood-diagonal-complex*))
  (mapcar (partial-1 #'+ pos) deltas))


(defparameter *nhood-d1-list* '((-1 0) (0 1) (1 0) (0 -1)))
(defparameter *nhood-diagonal-list* (concatenate 'list
                                                    *nhood-d1-list*
                                                    '((-1 1) (1 1) (1 -1) (-1 -1))))

(defmethod adjacents ((pos list)
                      &key include-diagonal
                      &aux (deltas *nhood-d1-list*))
  (when include-diagonal
    (setf deltas *nhood-diagonal-list*))
  (loop for d in deltas collect (mapcar #'+ pos d)))

(defun dijkstra (init-state &key (init-cost 0) goal-state (goalp 'void) neighbors
                            (test 'eql) state-key prune)
  (a* init-state
      :init-cost init-cost
      :goal-state goal-state
      :goalp goalp
      :neighbors neighbors
      :heuristic (constantly 0)
      :test test
      :state-key state-key
      :prune prune))

(defun a* (init-state &key (init-cost 0) goal-state (goalp 'void) neighbors
                      (heuristic (error "Heuristic is required"))
                      (test 'eql) state-key prune
                      &aux (cost-so-far (make-hash-table :test test))
                      (come-from (make-hash-table :test test)))
  (when goal-state (setf goalp (partial-1 test goal-state)))
  (macrolet ((priority (state-cost state)
               `(+ ,state-cost (funcall heuristic ,state)))
             (key (state)
               `(if state-key (funcall state-key ,state) ,state))
             (prunep (state cost)
               `(and prune (funcall prune ,state ,cost))))
    (let (best-state)
      (loop
        :with frontier = (make-hq :key #'cdr)
        :initially (progn
                     (setf (gethash (key init-state) cost-so-far) init-cost)
                     (hq-insert frontier (cons (cons init-state init-cost)
                                               (priority init-cost init-state))))
        :until (hq-empty-p frontier)
        :for (state . state-cost) = (car (hq-pop frontier))
        :when (funcall goalp state) :return (setf best-state state)
        :do (when (= (gethash (key state) cost-so-far) state-cost) ;; XXX is this check really needed?!?!?!?!
              (loop
                :for (next-state . cost) :in (funcall neighbors state)
                :for next-cost = (+ state-cost cost)
                :do (multiple-value-bind (existing-cost present-p)
                        (gethash (key next-state) cost-so-far)
                      (when (and (or (not present-p) (< next-cost existing-cost))
                                 (not (prunep next-state next-cost)))
                        (setf (gethash (key next-state) cost-so-far) next-cost
                              (gethash (key next-state) come-from) state)
                        (hq-insert frontier (cons (cons next-state next-cost)
                                                  (priority next-cost next-state))))))))
      (values
        best-state
        (and best-state (gethash (key best-state) cost-so-far))
        (search-backtrack come-from best-state)
        cost-so-far))))

(defun search-backtrack (come-from curr)
  (nreverse (recursively ((curr curr))
              (when curr
                (cons curr (recur (gethash curr come-from)))))))

(defmacro search-state (&body body) `(nth-value 0 ,@body))
(defmacro search-cost (&body body) `(nth-value 1 ,@body))
(defmacro search-path (&body body) `(nth-value 2 ,@body))
(defmacro search-costs-table (&body body) `(nth-value 3 ,@body))

(defun search-unit-cost (neighbors)
  (lambda (state)
    (mapcar (partial-1 #'cons _ 1) (funcall neighbors state))))

(defun bfs (init-state &key (init-cost 0) goal-state (goalp 'void) neighbors
                       (test 'eql) state-key prune
                       &aux (cost-so-far (make-hash-table :test test))
                       (come-from (make-hash-table :test test)))
  (when goal-state (setf goalp (partial-1 test goal-state)))
  (macrolet ((key (state)
               `(if state-key (funcall state-key ,state) ,state))
             (prunep (state cost)
               `(and prune (funcall prune ,state ,cost))))
    (let (best-state)
      (loop
        :with frontier = (enqueue init-state (make-queue))
        :initially (setf (gethash (key init-state) cost-so-far) init-cost)
        :until (queue-empty-p frontier)
        :for state = (dequeue frontier)
        :for state-cost = (gethash (key state) cost-so-far)
        :when (funcall goalp state) :return (setf best-state state)
        :do (loop
              :for next-state :in (funcall neighbors state)
              :for next-cost = (1+ state-cost)
              :do (multiple-value-bind (existing-cost present-p)
                      (gethash (key next-state) cost-so-far)
                    (declare (ignore existing-cost))
                    (when (and (not present-p)
                               (not (prunep next-state next-cost)))
                      (setf (gethash (key next-state) cost-so-far) next-cost
                            (gethash (key next-state) come-from) state)
                      (enqueue next-state frontier)))))
      (values
        best-state
        (and best-state (gethash (key best-state) cost-so-far))
        (search-backtrack come-from best-state)
        cost-so-far))))


(defun floyd (next init-state &key (copier 'identity) (key 'identity) (test 'eql))
  "Also called the 'tortoise and the hare algorithm',"
  (let (tortoise-state hare-state cycles-at cycle-size)
    (loop
      :initially (setf tortoise-state (funcall next (funcall copier init-state))
                       hare-state (funcall next (funcall copier tortoise-state)))
      :for tortoise = (funcall key tortoise-state)
      :for hare = (funcall key hare-state)
      :while (not (funcall test tortoise hare))
      :do (setf tortoise-state (funcall next tortoise-state)
                hare-state (funcall next (funcall next hare-state))))
    (loop
      :initially (setf tortoise-state (funcall copier init-state)
                       cycles-at 0)
      :for tortoise = (funcall key tortoise-state)
      :for hare = (funcall key hare-state)
      :while (not (funcall test tortoise hare))
      :do (setf tortoise-state (funcall next tortoise-state)
                hare-state (funcall next hare-state)
                cycles-at (1+ cycles-at)))
    (loop
      :with tortoise = (funcall key tortoise-state)
      :initially (setf hare-state (funcall next (funcall copier tortoise-state))
                       cycle-size 1)
      :for hare = (funcall key hare-state)
      :while (not (funcall test tortoise hare))
      :do (setf hare-state (funcall next hare-state)
                cycle-size (1+ cycle-size)))
    (list cycles-at cycle-size tortoise-state)))

(defun binary-search (lo hi next)
  (loop for mid = (floor (+ lo hi) 2)
        when (< hi lo) return (values mid nil) do
        (setf (values lo hi)
              (case (funcall next mid)
                (-1 (values (1+ mid) hi))
                (+1 (values lo (1- mid)))
                (0 (return (values mid t)))))))

;;;; Copy pasta ---------------------------------------------------------------

(defun mkstrs (sep &rest args)
  "Like `mkstr`, but concatenates with a given separator."
  (with-output-to-string (s)
    (format s (mkstr "~{~a~^" sep "~}") args)))

(defun mkstrc (&rest args)
  "Like `mkstr`, but concatenates with commas."
  (apply #'mkstrs #\, args))

;; XXX remove
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

  https://github.com/sjl/cl-losh/blob/master/DOCUMENTATION.markdown#gathering-macro
  "
  (with-gensyms (result)
    `(let ((,result nil))
       (flet ((gather (item)
                (push item ,result)
                item))
         ,@body)
       (nreverse ,result))))

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

(defun parse-char (string)
  "Return the first character of `string`"
  (char string 0))

(defun extract-positive-integers (s)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" s)))

(defun extract-integers (s)
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "-?\\d+" s)))

(defun extract-forms (s)
  (with-input-from-string (stream s)
    (uiop:slurp-stream-forms stream)))

;;;; Problems -----------------------------------------------------------------

(defun read-decrypted-problem-input (year day)
  (labels ((decrypt (ciphertext)
             (classified:decrypt (make-instance 'classified::encryptor)
                                 ciphertext
                                 :message-serializer
                                 (make-instance 'classified::message-serializer)
                                 :key-provider
                                 (classified::build-key-provider (uiop:getenv "PRIMARY_KEY")))))
    (ignore-errors
      (decrypt
        (with-standard-io-syntax
          (read-from-string
            (uiop:read-file-string (problem-input-path year day "enc"))))))))
#+#:excluded (read-decrypted-problem-input 2023 25)
#+#:excluded (time
               (dorangei (year 2015 2023)
                 (dorangei (day 1 25)
                   (read-decrypted-problem-input year day))))

(defun encrypt-problem-input (year day)
  (labels ((encrypt (plaintext)
             (classified:encrypt (make-instance 'classified::encryptor)
                                 plaintext
                                 :message-serializer
                                 (make-instance 'classified::message-serializer)
                                 :key-provider
                                 (classified::build-key-provider (uiop:getenv "PRIMARY_KEY")))))
    (with-open-file (stream (problem-input-path year day "enc")
                            :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (prin1 (encrypt (uiop:read-file-string (problem-input-path year day "txt")))
               stream)))
    (read-decrypted-problem-input year day)))
#+#:excluded (encrypt-problem-input 2023 25)
#+#:excluded (time
               (dorangei (year 2015 2023)
                 (dorangei (day 1 25)
                   (encrypt-problem-input year day))))

(defun read-problem-input (year day)
  (let ((classified:*key-derivation-salt* (uiop:getenv "KEY_DERIVATION_SALT")))
    (cl-ppcre:split "\\n" (or (read-decrypted-problem-input year day)
                              (encrypt-problem-input year day)))))
#+#:excluded (read-problem-input 2023 25)
#+#:excluded (time
               (dorangei (year 2015 2023)
                 (dorangei (day 1 25)
                   (read-problem-input year day))))


(defmacro define-solution ((year day)
                           (arg &optional (reader 'identity))
                           &body body)
  (let ((run (symb 'solution-run)))
    `(defun ,run ()
       (let ((,arg (,reader (read-problem-input ,year ,day))))
         ,@body))))
(defun problem-input-path (year day &optional (ext "txt"))
  (make-pathname :directory `(:relative "src" ,(format NIL "~A" year))
                 :name (format nil "day~2,'0D" day)
                 :type ext))
#+#:excluded (problem-input-path 2023 25)
#+#:excluded (problem-input-path 2023 25 "enc")


(defvar *tests* nil "A list of tests; the default argument to RUN.")

(defun run () (1am:run *tests*))

(defmacro deftest (name &body body)
  "Define a test function and add it to `*tests*`."
  `(progn
     (let ((1am:*tests* nil))
       (1am:test ,name ,@body))
     (pushnew ',name *tests*)))

(defmacro define-test ((year day) (expected-part1 &optional expected-part2))
  (let ((package-name (symb (format nil "AOC/~D/~2,'0D" year day)))
        (test-name (symb (format nil "TEST-~D/~2,'0D" year day)))
        (test-runner-name (symb 'test-run))
        (runner-name (symb 'solution-run)))
    `(values
      (deftest ,test-name
        (let ((*package* (find-package ',package-name)))
          (multiple-value-bind (actual-part1 actual-part2) (,runner-name)
            (1am:is (equal ,expected-part1 actual-part1))
            (when actual-part2
              (1am:is (equal ,expected-part2 actual-part2)))
            (values))))
      (defun ,test-runner-name ()
        (,test-name)))))
