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

(defun maximization (x &key (key 'identity))
  "Returns the max of all the elements of `x`, or NIL if `x` is _empty_.

  If `key` is specified, this function will return the max of all
  the values of `x`, `map`-ed using `key`."
  (loop
    :for e :being :the :elements :of x
    :for v = (funcall key e)
    :maximizing v))

(defun minimization (x &key (key 'identity))
  "Returns the min of all the elements of `x`, or NIL if `x` is _empty_.

  If `key` is specified, this function will return the min of all
  the values of `x`, `map`-ed using `key`."
  (loop
    :for e :being :the :elements :of x
    :for v = (funcall key e)
    :minimizing v))

(defun minimizing (x &key (key 'identity))
  "Returns the element which is the min of all the elements of `x`, or NIL if
  `x` is _empty_.

  If `key` is specified, this function will return the element that
  minimizes `x`."
  (loop
    :with best-e
    :with best-v
    :for e :being :the :elements :of x
    :for v = (funcall key e)
    :when (or (not best-e) (< v best-v))
    :do (setf best-e e
              best-v v)
    :finally (return best-e)))

(defun maximizing (x &key (key 'identity))
  "Returns the element which is the max of all the elements of `x`, or NIL if
  `x` is _empty_.

  If `key` is specified, this function will return the element that
  maximizes `x`."
  (loop
    :with best-e
    :with best-v
    :for e :being :the :elements :of x
    :for v = (funcall key e)
    :when (or (not best-e) (> v best-v))
    :do (setf best-e e
              best-v v)
    :finally (return best-e)))

(defun dividesp (divisor number)
  "Returns `T` if `DIVISOR` divies `NUMBER`."
  (zerop (rem number divisor)))

(defun alphabet ()
  (loop
    :with from = (char-code #\a)
    :with to = (char-code #\z)
    :for code :from from :to to
    :collecting (code-char code)))

(defmacro aesthetic-string (data)
  `(format NIL "~A" ,data))

(defmacro hexadecimal-string (seq)
  `(format NIL "~{~(~2,'0x~)~}" ,seq))

(defmacro hexadecimal-binary (s)
  `(let* ((len (* 4 (length ,s)))
          (control-str (mkstr "~" len ",'0b"))
          (num (parse-integer ,s :radix 16)))
     (format NIL control-str num)))

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
              (hash-table-insert freqs c 1)
              (incf (gethash c freqs)))
        :finally (return freqs)))

(defun group-by (x &key (key 'identity) (test 'eql))
  "Groups the elements of `x` based on the values returned by applying function
  `key` to such elements.

  `key` is a function computing a key value for each element, and based on that
  value groups are created, and `test` instead, is the equality function used to
  compare key values together.

  Note: this function aggregates common elements regardless of their input
  order.

  Examples:

    (group-by '(1 2 3 4 5 6) :key #'oddp)
    =>
    (((5 3 1) T) ((6 4 2) NIL))
  "
  (loop
    :with groups = (make-hash-table :test test)
    :with keys = (make-hash-table :test test)
    :for k :being :the :elements :of x
    :for v = (funcall key k)
    :do (push k (gethash v groups))
    :unless (gethash v keys) :do (hash-table-insert keys v T) :and :collect v :into sorted-keys ; XXX hash-set
    :finally (return (loop
                       :for k :in sorted-keys
                       :for group = (gethash k groups)
                       :collect (list group k)))))

(defun unique-only (x &key (key 'identity) (test 'eql))
  "Remove from `X` all the elements which are not unique.

  This differs from REMOVE-DUPLICATES in that if a element has duplicates, not just
  the duplicates but the element itself is removed.
  "
  (loop
    :for (group) :in (group-by x :key key :test test)
    :when (= 1 (length group)) :collect (first group)))

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

;;; https://stackoverflow.com/a/8448611/348524
(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))


;;;; Functional --------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-contain-placeholder-p (args placeholder)
    (recursively ((args args))
      (if (atom args)
        (string= args placeholder)
        (or (recur (car args))
            (recur (cdr args)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-replace (placeholder args name)
    (subst name placeholder args)))

(defun args-prepend (args name)
  (cons name args))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-append (args name)
    (append args (list name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-replace-placeholder-or-append (args placeholder name)
    (if (args-contain-placeholder-p args placeholder)
      (args-replace placeholder args name)
      (args-append args name))))

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
  (with-gensyms (memo result result-exists-p)
    `(let ((,memo (make-hash-table :test 'equalp)))
       (defun ,name ,args
         (multiple-value-bind (,result ,result-exists-p)
             (gethash ,(first args) ,memo)
           (if ,result-exists-p
             ,result
             (setf (gethash ,(first args) ,memo) (progn ,@body))))))))

;;;; Math --------------------------------------------------------------------

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

(defun modn (number divisor)
  (if (>= number 0)
    (mod number divisor)
    (- (mod number divisor) divisor)))

(defun digits-reverse (n &optional (base 10))
  "Similar to Quickutil's DIGITS, except digits are returned in reading order.

For example, where (digits 123) would return (3 2 1), DIGITS-REVERSE would return instead (1 2 3)"
  (nreverse (digits n base)))

(defun str-digits (string &optional (type 'list))
  "Converts `string` -- a string representation of a natural number -- into a sequence of its digits (in reading order).

By default, it will store the result into a list, but `type` can be tweaked to change that"
  (map type #'(lambda (c) (- (char-code c) (char-code #\0))) string))

;;;; Control flow ------------------------------------------------------------

(defmacro recursively (bindings &body body)
  "Execute `body` recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) starting values.

  In `body` the symbol `recur` will be bound to the function for recurring."
  (let ((names (mapcar #'(lambda (b) (if (atom b) b (first b))) bindings))
        (values (mapcar #'(lambda (b) (if (atom b) nil (second b))) bindings)))
    `(labels ((recur (,@names)
                ,@body))
      (recur ,@values))))

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

;;;; Hash tables --------------------------------------------------------------

(defun hash-table-find (value h &key (key 'identity) (test 'eql))
  "Returns the first key in `h`, whose value is equal to `value`"
  (loop
    :for k :being :the :hash-keys :of h
    :for v = (gethash k h)
    :when (funcall test value (funcall key v)) :return (values k v)))

(defmacro hash-table-insert (ht key value)
  `(setf (gethash ,key ,ht) ,value))

(defun hash-table-contains-key-p (h key)
  (multiple-value-bind (v containsp)
      (gethash key h)
    (declare (ignore v))
    containsp))

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
  (let ((min-x (minimization (hash-table-keys h) :key #'realpart))
        (max-x (maximization (hash-table-keys h) :key #'realpart))
        (min-y (minimization (hash-table-keys h) :key #'imagpart))
        (max-y (maximization (hash-table-keys h) :key #'imagpart)))
    (doirange (y max-y min-y -1)
      (doirange (x min-x max-x)
        (let ((pos (complex x y)))
          (format stream "~a" (funcall key (gethash pos h) pos))))
      (format stream "~&"))))

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

(defun distinct-disjointsets (sets)
  (remove-duplicates sets
                     :key #'disjointset-find
                     :test 'eq))

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

;;;; Poor's Heap Queue --------------------------------------------------------

(defun make-hq ()
  "Create an instance of Poor's Heap Queue."
  NIL)

(defun hq-empty-p (hq)
  "Returns T if HQ is empty"
  (not hq))

(defmacro hq-popf (hq)
  "Pop the element with `HQ` with the lowest priority."
  (with-gensyms (priority item)
    `(destructuring-bind (,priority ,item)
         (pop ,hq)
       (values ,item ,priority))))

(define-modify-macro hq-insertf (item priority)
  (lambda (hq item priority)
    (merge 'list hq (list (list priority item)) #'< :key #'first))
  "Add `ITEM` (with priority `PRIORITY`) to the heap queue.

  Also, it internally uses MERGE, which means is not as bad as running
  SORT on each insert, but still, we could make things run faster.")

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

(defun adjacents (pos &key include-diagonal
                      &aux (deltas (list #C(0 1) #C(1 0) #C(0 -1) #C(-1 0))))
  "Return all the adjacents positions of POS.

  By default, only the 4 immediate adjacents positions are returned
  (i.e. top, right, bottom, and left), but INCLUDE-DIAGONAL can be used to
  also return diagonal positions too."
  (assert (or (complexp pos) (numberp pos)))
  (when include-diagonal
    (setf deltas (nconc deltas (list #C(1 1) #C(1 -1) #C(-1 -1) #C(-1 1)))))
  (loop
    :for d :in deltas
    :collect (+ pos d)))

(defun a-star (init-state &key (init-cost 0) goal-state goalp neighbors
                          (heuristic (constantly 0)) (test 'eql) print-stats
                          &aux (cost-so-far (make-hash-table :test test))
                          (come-from (make-hash-table :test test)))
  (when goal-state
    (setf goalp (partial-1 test goal-state)))
  (flet ((calc-priority (state &aux (cost (gethash state cost-so-far)))
           (+ cost (funcall heuristic state))))
    (hash-table-insert cost-so-far init-state init-cost)
    (values
      (loop
        :with processed-states = 0
        :with max-queued-states = 0
        :with frontier = (make-hq)
        :initially (hq-insertf frontier (list init-state init-cost) (calc-priority init-state))
        :until (hq-empty-p frontier)
        :for (state state-cost) = (hq-popf frontier)
        :when (funcall goalp state) :return (prog1 state
                                              (when print-stats
                                                (format t "Processed states: ~d~&Max queued states: ~d~%" processed-states max-queued-states)))
        :do (when (= state-cost (gethash state cost-so-far))
              (dolist (next (funcall neighbors state state-cost))
                (destructuring-bind (next-state next-cost) next
                  (multiple-value-bind (existing-cost present-p) (gethash next-state cost-so-far)
                    (when (or (not present-p) (< next-cost existing-cost))
                      (hash-table-insert cost-so-far next-state next-cost)
                      (hash-table-insert come-from next-state state)
                      (hq-insertf frontier (list next-state next-cost) (calc-priority next-state))))))
              (when print-stats
                (setf processed-states (1+ processed-states)
                      max-queued-states (max max-queued-states (length frontier)))))
        :finally (when print-stats
                   (format t "Processed states: ~d~&Max queued states: ~d~%" processed-states max-queued-states)))
      cost-so-far
      come-from)))

(defun a-star-backtrack (come-from curr)
  (nreverse (recursively ((curr curr))
              (when curr
                (cons curr (recur (gethash curr come-from)))))))

(defun a-star-neighbors-cost-auto-increment (neighbors)
  (lambda (state cost)
    (mapcar
      (lambda (next-state)
        (list next-state (1+ cost)))
      (funcall neighbors state))))

(defun bfs (init-state &key (init-cost 0) goal-state (goalp #'void) neighbors
                       (test 'eql) print-stats
                       &aux (cost-so-far (make-hash-table :test test))
                       (come-from (make-hash-table :test test)))
  "XXX"
  (when goal-state
    (setf goalp (partial-1 test goal-state)))
  (hash-table-insert cost-so-far init-state init-cost)
  (values
    (loop
      :with frontier = (enqueue init-state (make-queue))
      :with processed-states = 0
      :with max-queued-states = 0
      :until (queue-empty-p frontier)
      :for state = (dequeue frontier)
      :for state-cost = (gethash state cost-so-far)
      :when (funcall goalp state) :return (prog1 state
                                            (when print-stats
                                              (format t "Processed states: ~d~&Max queued states: ~d~%" processed-states max-queued-states)))
      :do (progn
            (dolist (next-state (funcall neighbors state))
              (unless (gethash next-state cost-so-far)
                (hash-table-insert cost-so-far next-state (1+ state-cost))
                (hash-table-insert come-from next-state state)
                (enqueue next-state frontier)))
            (when print-stats
              (setf processed-states (1+ processed-states)
                    max-queued-states (max max-queued-states (length (queue-items frontier))))))
      :finally (when print-stats
                 (format t "Processed states: ~d~&Max queued states: ~d~%" processed-states max-queued-states)))
    cost-so-far
    come-from))

(defun dijkstra (init-state init-cost target-state neighbors)
  (a-star
    init-state
    init-cost
    target-state
    neighbors
    (lambda (state) (declare (ignore state)) 0)))

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

(defun binary-search (min-state max-state fun)
  "XXX"
  (recursively ((min-state min-state)
                (max-state max-state))
    (let ((mid-state (floor (+ max-state min-state) 2)))
      (if (= min-state mid-state)
        (values mid-state NIL)
        (case (funcall fun mid-state)
          (-1 (recur  mid-state max-state))
          (0 (return-from binary-search (values mid-state T)))
          (1 (recur min-state mid-state)))))))

;;;; Copy pasta ---------------------------------------------------------------

(defun mkstrc (&rest args)
  "Like `mkstr`, but concatenates with commas."
  (with-output-to-string (s)
    (format s "~{~a~^,~}" args)))

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

;;;; Iterators ----------------------------------------------------------------

(defmacro dorange ((var from to &optional (delta 1)) &body body)
  "Perform `body` on the given range of values.
  During iteration `body` will be executed with `var` bound to successive values
  in the range [`from`, `to`).

  Example:
    (dorange (x 5 8)
      (pr x))
    ; =>
    5
    6
    7
  "
  (with-gensyms (lto ldelta comp)
    `(let* ((,lto ,to)
            (,ldelta ,delta)
            (,comp (if (> ,ldelta 0) '< '>)))
        (loop
          :for ,var = ,from :then (+ ,var ,ldelta)
          :while (funcall ,comp ,var ,lto)
          :do (progn ,@body)))))

(defmacro doirange ((var from to &optional (delta 1)) &body body)
  "Similar to `DORANGE`, but `TO` is now included in the range."
  (let ((to-form (list (if (> delta 0) '1+ '1-) to)))
    `(dorange (,var ,from ,to-form ,delta)
      ,@body)))

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
  (with-gensyms (file)
    (let ((run (symb 'problem-run)))
      `(defun ,run (&optional ,arg)
         (let ((,file (open (problem-input-path ,year ,day))))
           (unwind-protect
               (progn (setf ,arg (,reader (read-all ,file)))
                      ,@body)
             (when ,file (close ,file))))))))

(defun problem-input-path (year day)
  (make-pathname
    :directory `(:relative "." ,(aesthetic-string year))
    :name (format nil "day~2,'0D" day)
    :type "txt"))
