(defun read-by-line (in)
  (loop for i = (read-line in nil :eof)
      until (eq i :eof)
      collect i))


(defun list-to-string (lst)
  "Convert a list into a string of concatenated characters"
  (format nil "~{~A ~}" lst))


(defun substringp (needle haystack &key (test 'char=))
  "Returns the index of the first occurrence of the string designated
  by NEEDLE within the string designated by HAYSTACK, or NIL if it does
  not occur.  Characters within the string are compared by TEST, which
  defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))

(defun parse-time-log (str)
  (let* ((date (subseq str 1 11))
         (minute (parse-integer (subseq str 15 17)))
         (guard (if (substringp "Guard" str)(parse-integer (subseq str 26) :junk-allowed T)))
         (falls-asleep (substringp "falls asleep" str))
         (wakes-up (substringp "wakes up" str)))
    (list date minute guard falls-asleep wakes-up)))


(defun record-guard-asleep (guard fall-asleep-minute wake-up-minute date time-schedule)
  (loop for m from fall-asleep-minute to (- wake-up-minute 1)
        do (progn
             (if (not (gethash guard time-schedule))
               (setf (gethash guard time-schedule) (make-hash-table)))
             (let ((guard-entry (gethash guard time-schedule)))
               (setf (gethash m guard-entry)
                 (cons date
                       (gethash m guard-entry)))))))


(defun hash-to-list (hash &key map)
  (loop for key being the hash-keys of hash
        collect (funcall map (gethash key hash) key)))


; Returns a hash-table having Guard IDs as keys, and another hash-table as value
; (this one having midnight minutes as keys, and as values a list containing
; the dates where the guard fell asleep))")
(defun populate-time-schedule (time-logs)
  (let ((time-schedule (make-hash-table))
        (guard)
        (fall-asleep-minute))
    (loop for entry in time-logs
          do (cond
               ((third entry) (setf guard (third entry)))
               ((fourth entry) (setf fall-asleep-minute (second entry)))
               ((fifth entry) (record-guard-asleep guard
                                                   fall-asleep-minute
                                                   (second entry)
                                                   (first entry)
                                                   time-schedule))))
    time-schedule))


(defun count-minutes-asleep (minutes-log)
  (reduce
    #'+
    (hash-to-list minutes-log
                  :map (lambda (dates minute)
                         (length dates)))))


(defun solve-day4-1 (time-logs)
  (let* ((time-schedule (populate-time-schedule time-logs))
         (asleep-guards (hash-to-list time-schedule
                                      :map (lambda (minutes-log guard)
                                             (list
                                               guard
                                               (count-minutes-asleep minutes-log)))))
         (target-guard (first
                         (sort
                           asleep-guards
                           #'>
                           :key #'second)))
         (guard-minutes-log (hash-to-list (gethash (first target-guard) time-schedule)
                                          :map (lambda (dates minute)
                                                 (list minute dates))))
         (most-asleep-minute (first (sort
                                      guard-minutes-log
                                      #'>
                                      :key (lambda (entry) (length (second entry)))))))
    (*
      (first target-guard)
      (first most-asleep-minute))))


(defun day4-1 ()
  (let* ((in (open "./day4.input"))
         (lst (read-by-line in))
         (result (solve-day4-1
                   (mapcar #'parse-time-log
                           (sort lst #'string<)))))
    (close in)
    result))


(defun max-dates-per-asleep-minute (minutes-log)
  (first
    (sort
      (hash-to-list minutes-log
                    :map (lambda (dates minute)
                          (list
                            minute
                            (length dates))))
      #'>
      :key #'second)))


(defun solve-day4-2 (time-logs)
  (let* ((time-schedule (populate-time-schedule time-logs))
         (asleep-guards (hash-to-list time-schedule
                                      :map (lambda (minutes-log guard)
                                             (list
                                               guard
                                               (max-dates-per-asleep-minute minutes-log)))))
         (target-guard (first
                         (sort
                           asleep-guards
                           #'>
                           :key (lambda (entry) (second (second entry)))))))
    (*
      (first target-guard)
      (first (second target-guard)))))


(defun day4-2 ()
  (let* ((in (open "./day4.input"))
         (lst (read-by-line in))
         (result (solve-day4-2
                   (mapcar #'parse-time-log
                           (sort lst #'string<)))))
    (close in)
    result))


(defun same-type-opposite-polarityp (u1 u2)
  (and
    (char-equal u1 u2)
    (not (char= u1 u2))))


(defun reduce-polymer (seq)
  (cond
    ((< (length seq) 2) seq)
    ((same-type-opposite-polarityp (first seq) (second seq))
     (reduce-polymer (cddr seq)))
    (T (cons
         (car seq)
         (reduce-polymer (cdr seq))))))


(defun react-polimer (polymer)
  (let ((next (reduce-polymer polymer)))
    (if (= (length next) (length polymer))
      next
      (react-polimer next))))


(defun solve-day5-1 (polymer)
  (length (react-polimer polymer)))


(defun day5-1 ()
  (let* ((in (open "./day5.input"))
         (str (first (read-by-line in)))
         (result (solve-day5-1
                   (coerce str 'list))))
    (close in)
    result))

(defun lowercase-alphabet-string ()
  (loop for c from (char-code #\a) to (char-code #\z)
        collect (code-char c)))


(defun solve-day5-2 (polymer)
  (length
    (first
      (sort
        (loop for c in (lowercase-alphabet-string)
              collect (react-polimer (remove-if
                                       (lambda (u) (char-equal u c))
                                      polymer)))
        #'<
        :key #'length))))


(defun day5-2 ()
  (let* ((in (open "./day5.input"))
         (str (first (read-by-line in)))
         (result (solve-day5-2
                   (coerce str 'list))))
    (close in)
    result))


(defun parse-coords (str)
  (let* ((parts (split-sequence:split-sequence #\Space str)))
    (list
      (parse-integer (first parts) :junk-allowed T)
      (parse-integer (second parts)))))


(defun manhattan-distance (x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs (- y2 y1))))


(defun closest-coords (x y places)
  (let* ((sorted (sort (copy-seq places) #'<
                       :key (lambda (places)
                              (manhattan-distance x y (first places) (second places)))))
         (closest1 (first sorted))
         (closest2 (second sorted)))
    (if (= (manhattan-distance x y (first closest1) (second closest1))
           (manhattan-distance x y (first closest2) (second closest2)))
      (list closest1 closest2)
      (list closest1))))


(defun color-grid-point (x y places grid)
  (let ((closest (closest-coords x y places)))
    (if (= 1 (length closest))
      (setf (gethash (list-to-string (list x y)) grid)
            (list-to-string (first closest))))))


(defun colored-grid (minx maxx miny maxy places)
  (let ((grid (make-hash-table :test 'equalp)))
    (loop for x from minx to maxx
          do (loop for y from miny to maxy
                    do (color-grid-point x y places grid)))
    grid))


(defun on-the-edge (minx maxx miny maxy grid)
  (let ((edges (make-hash-table :test 'equalp)))
    (loop for x from minx to maxx
          do (let ((key (list-to-string (list x miny))))
               (if (gethash key grid)
                 (setf (gethash (gethash key grid) edges) T))))
    (loop for x from minx to maxx
          do (let ((key (list-to-string (list x maxy))))
               (if (gethash key grid)
                 (setf (gethash (gethash key grid) edges) T))))
    (loop for y from miny to maxy
          do (let ((key (list-to-string (list minx y))))
               (if (gethash key grid)
                 (setf (gethash (gethash key grid) edges) T))))
    (loop for y from miny to maxy
          do (let ((key (list-to-string (list maxx y))))
               (if (gethash key grid)
                 (setf (gethash (gethash key grid) edges) T))))
    edges))


(defun solve-day6-1 (places)
  (let* ((xs (mapcar #'first places))
         (ys (mapcar #'second places))
         (minx (apply 'min xs))
         (maxx (apply 'max xs))
         (miny (apply 'min ys))
         (maxy (apply 'max ys))
         (grid (colored-grid minx maxx miny maxy places))
         (edges (on-the-edge minx maxx miny maxy grid))
         (sums-by-anchor (make-hash-table :test 'equalp)))
    (loop for key being the hash-keys of grid
          do (let ((color (gethash key grid)))
               (setf (gethash color sums-by-anchor)
                     (cons key (gethash color sums-by-anchor)))))
    (first
      (sort
        (loop for key being the hash-keys of sums-by-anchor
              unless (gethash key edges)
              collect (length (gethash key sums-by-anchor)))
        #'>))))


(defun day6-1 ()
  (let* ((in (open "./day6.input"))
         (lst (read-by-line in))
         (result (solve-day6-1
                   (mapcar 'parse-coords lst))))
    (close in)
    result))


(defun sum-of-all-distances (x y places)
  (loop for coords in places
        summing (manhattan-distance x y (first coords) (second coords))))


(defun solve-day6-2 (places max-distance)
  (let* ((xs (mapcar #'first places))
         (ys (mapcar #'second places))
         (minx (apply 'min xs))
         (maxx (apply 'max xs))
         (miny (apply 'min ys))
         (maxy (apply 'max ys)))
    (length
      (loop for x from minx to maxx
            append (loop for y from miny to maxy
                        when (< (sum-of-all-distances x y places) max-distance)
                        collect it)))))


(defun day6-2 ()
  (let* ((in (open "./day6.input"))
         (lst (read-by-line in))
         (result (solve-day6-2
                   (mapcar 'parse-coords lst)
                   10000)))
    (close in)
    result))


(defun parse-requirement (str)
  (let* ((parts (split-sequence:split-sequence #\Space str)))
    (list
      (second parts)
      (eighth parts))))

(defun build-dependency-list (requirements)
  (let ((unblocks (make-hash-table :test 'equal))
        (blocked-by (make-hash-table :test 'equal)))
    (loop for req in requirements
          do (setf (gethash (first req) unblocks)
                   (cons (second req)
                         (gethash (first req) unblocks))
                   (gethash (second req) blocked-by)
                   (cons (first req)
                         (gethash (second req) blocked-by))))
    (list unblocks blocked-by)))

(defun unique (lst)
  (remove-duplicates lst :test 'equal))

(defun dependency-roots (requirements blocked-by)
  (remove-duplicates
    (remove-if
      #'(lambda (n) (gethash n blocked-by))
      (mapcar #'first requirements))
    :test 'equal))

(defun is-unblocked (curr unblocked blocked-by)
  (let ((dependencies (gethash curr blocked-by)))
    (every
      #'(lambda (dep) (member dep unblocked :test 'equal))
      dependencies)))

(defun solve-day7-1 (requirements)
  (let* ((data (build-dependency-list requirements))
         (unblocks (first data))
         (blocked-by (second data))
         (to-process (dependency-roots requirements blocked-by))
         visited)
    (loop :while to-process
          :do (setf to-process (sort to-process #'string<))
          :do (let ((curr (pop to-process)))
                (push curr visited)
                (loop :for req :in (gethash curr unblocks)
                      :when (is-unblocked req visited blocked-by)
                      :do (setf to-process (cons req to-process)))))
    (format NIL "~{~a~}" (reverse visited))))

(defun day7-1 ()
  (let* ((in (open "./day7.input"))
         (lst (read-by-line in))
         (result (solve-day7-1
                   (mapcar 'parse-requirement lst))))
    (close in)
    result))

(defun requirement-construction-time (req offset)
  (+ offset
     1
     (- (char-int (char req 0))
        (char-int #\A))))

(defun solve-day7-2 (workers offset requirements)
  (let* ((data (build-dependency-list requirements))
         (unblocks (first data))
         (blocked-by (second data))
         (to-process (dependency-roots requirements blocked-by))
         in-the-making
         visited
         (elapsed 0))
    (loop :while (or to-process in-the-making)
          :do (setf to-process (sort to-process #'string<))
          :do (let* ((to-pop (- workers (length in-the-making)))
                     (popped (mapcar
                               #'(lambda (r)
                                   (list r (requirement-construction-time r offset)))
                               (loop :for i :from 0 :below (min to-pop (length to-process))
                                     :collecting (pop to-process)))))
                (setf in-the-making (sort (append popped in-the-making) #'< :key #'second)))
          :do (let* ((item (pop in-the-making))
                     (curr (first item))
                     (remaining-time (second item)))
                (loop :for i :from 0 :below (min (- workers 1) (length in-the-making))
                      :do (decf (second (nth i in-the-making)) remaining-time))
                (incf elapsed remaining-time)
                (push curr visited)
                (loop :for req :in (gethash curr unblocks)
                      :when (is-unblocked req visited blocked-by)
                      :do (setf to-process (cons req to-process)
                                to-process (sort to-process #'string<)))))
    elapsed))


(defun day7-2 ()
  (let* ((in (open "./day7.input"))
         (lst (read-by-line in))
         (result (solve-day7-2 5
                               60
                               (mapcar 'parse-requirement lst))))
    (close in)
    result))


(defun parse-tree (lst)
  (let ((remaining (copy-seq lst)))
    (labels ((recurse ()
              (let ((nchildren (pop remaining))
                    (nmetadata (pop remaining))
                    (children ()))
                (loop for i from 1 to nchildren
                      do (push (recurse) children))
                (list
                  (nreverse children)
                  (loop for i from 1 to nmetadata
                        collect (pop remaining))))))
        (recurse))))


(defun solve-day8-1 (lst)
  (labels ((solve (tree)
             (let ((children (first tree))
                   (metadata (second tree)))
               (+
                 (loop for child in children
                       summing (solve child))
                 (loop for metadata in metadata
                       summing metadata)))))
      (solve (parse-tree lst))))


(defun day8-1 ()
  (let* ((in (open "./day8.input"))
         (str (first (read-by-line in)))
         (result (solve-day8-1 (mapcar #'parse-integer
                                       (split-sequence:split-sequence #\Space str)))))
    (close in)
    result))


(defun solve-day8-2 (lst)
  (labels ((solve (tree)
             (let ((children (first tree))
                   (metadata (second tree)))
               (if (null children)
                 (reduce #'+ metadata)
                 (loop for i in metadata
                       when (nth (- i 1) children)
                       summing (solve (nth (- i 1) children)))))))
      (solve (parse-tree lst))))


(defun day8-2 ()
  (let* ((in (open "./day8.input"))
         (str (first (read-by-line in)))
         (result (solve-day8-2 (mapcar #'parse-integer
                                       (split-sequence:split-sequence #\Space str)))))
    (close in)
    result))


(defun parse-marble-setup (str)
  (let ((parts (split-sequence:split-sequence #\Space str)))
    (list
      (parse-integer (first parts))
      (parse-integer (seventh parts)))))


(defun circular (items)
  "(setf *print-circle* t)"
  (setf (cdr (last items)) items)
  items)


(defun circular-push (item position circle)
  (let ((before (nthcdr position circle))
        (after (nthcdr (+ 1 position) circle)))
    (setf (cdr before)
          (cons item after))))


(defun circular-pop (position circle)
  (let ((before (nthcdr (- position 1) circle))
        (after (nthcdr (+ position 1) circle)))
    (setf (cdr before) after)))


(defun play-marbles (next circle size last-marble nplayers scores)
  (if (> next last-marble)
    scores
    (if (not (= 0 (mod next 23)))
      (play-marbles (+ 1 next)
                    (circular-push next 1 circle)
                    (+ 1 size)
                    last-marble
                    nplayers
                    scores)
      (let* ((current-player (mod next nplayers))
             (to-remove-index (- size 7))
             (to-remove (nth to-remove-index circle)))
        (setf (gethash current-player scores)
              (+
                (gethash current-player scores 0)
                next
                to-remove))
        (play-marbles (+ 1 next)
                      (circular-pop to-remove-index circle)
                      (- size 1)
                      last-marble
                      nplayers
                      scores)))))


(defun solve-day9-1 (lst multiplier)
  (let* ((nplayers (first lst))
         (last-marble (* multiplier (second lst)))
         (scores (play-marbles 1
                               (circular (list 0))
                               1
                               last-marble
                               nplayers
                               (make-hash-table))))
    (first
      (sort
        (hash-to-list scores :map (lambda (score player) score))
        #'>))))


(defun day9-1 ()
  (let* ((in (open "./day9.input"))
         (str (first (read-by-line in)))
         (result (solve-day9-1 (parse-marble-setup str)
                               1)))
    (close in)
    result))


(defun day9-1 ()
  (let* ((in (open "./day9.input"))
         (str (first (read-by-line in)))
         (result (solve-day9-1 (parse-marble-setup str)
                               100)))
    (close in)
    result))


(defun parse-star (str)
  (let* ((parts (split-sequence:split-sequence #\< str))
         (coordsstr (second parts))
         (speedstr (third parts))
         (coords-parts (split-sequence:split-sequence #\, coordsstr))
         (speed-parts (split-sequence:split-sequence #\, speedstr)))
    (list
      (parse-integer (first coords-parts) :junk-allowed T)
      (parse-integer (second coords-parts) :junk-allowed T)
      (parse-integer (first speed-parts) :junk-allowed T)
      (parse-integer (second speed-parts) :junk-allowed T))))


(defun sky-area (stars)
  (let* ((xs (mapcar #'first stars))
         (ys (mapcar #'second stars))
         (minx (apply 'min xs))
         (maxx (apply 'max xs))
         (miny (apply 'min ys))
         (maxy (apply 'max ys)))
    (* (- maxx minx)
       (- maxy miny))))


(defun print-sky (stars)
  (let* ((xs (mapcar #'first stars))
         (ys (mapcar #'second stars))
         (minx (apply 'min xs))
         (maxx (apply 'max xs))
         (miny (apply 'min ys))
         (maxy (apply 'max ys))
         (grid (make-hash-table :test 'equalp)))
    (loop for star in stars
          do (setf (gethash (list-to-string (list (first star) (second star))) grid) t))
    (loop for y from miny to maxy
          do (loop for x from minx to maxx
                   do (if (gethash (list-to-string (list x y)) grid)
                        (format t "#")
                        (format t "."))
                   finally (format t "~%")))))


(defun move-stars (stars)
  (mapcar (lambda (star)
            (list
              (+ (first star) (third star))
              (+ (second star) (fourth star))
              (third star)
              (fourth star)))
          stars))


(defun solve-day10-1 (stars)
  (let* ((sky-area-min (sky-area stars))
         (stars-next (move-stars stars))
         (sky-area-next (sky-area stars-next)))
    (loop while (< sky-area-next sky-area-min)
          do (setf stars stars-next
                   sky-area-min sky-area-next
                   stars-next (move-stars stars)
                   sky-area-next (sky-area stars-next)))
    (print-sky stars)))


(defun day10-1 ()
  (let* ((in (open "./day10.input"))
         (lst (read-by-line in))
         (result (solve-day10-1 (mapcar #'parse-star lst))))
    (close in)
    result))


(defun solve-day10-2 (stars)
  (let* ((sky-area-min (sky-area stars))
         (stars-next (move-stars stars))
         (sky-area-next (sky-area stars-next)))
    (loop while (< sky-area-next sky-area-min)
          do (setf stars stars-next
                   sky-area-min sky-area-next
                   stars-next (move-stars stars)
                   sky-area-next (sky-area stars-next))
          summing 1)))


(defun day10-2 ()
  (let* ((in (open "./day10.input"))
         (lst (read-by-line in))
         (result (solve-day10-2 (mapcar #'parse-star lst))))
    (close in)
    result))


(defun cell-power-level (x y serial-number)
  (let* ((rack-id (+ x 10))
         (power (* (+ (* rack-id y) serial-number) rack-id)))
    (- (mod (floor power 100) 10) 5)))


(defun create-power-grid (size serial-number)
  (let ((power-grid (make-array (list (+ size 2) (+ size 2)))))
    (loop :for y :from 1 :to size
          :do (loop :for x :from 1 :to size
                    :do (setf (aref power-grid y x) (cell-power-level x y serial-number))))
    power-grid))

(defun square-power (x y size power-grid)
  (loop :for yy :from y :below (+ y size)
        :summing (loop :for xx :from x :below (+ x size)
                       :summing (aref power-grid yy xx))))

(defun max-square-of-energy (size grid-size power-grid)
  (let (largest-power largest-power-square)
    (loop :for y :from 1 :to (+ (- grid-size size) 1)
          :do (loop :for x :from 1 :to (+ (- grid-size size) 1)
                    :do (let ((power (square-power x y size power-grid)))
                          (if (or (not largest-power)
                                  (> power largest-power))
                            (setf largest-power-square (list x y size)
                                  largest-power power)))))
    (list largest-power-square largest-power)))

(defun solve-day11-1 (serial-number)
  (let* ((power-grid (create-power-grid 300 serial-number))
         (largest-square (first (max-square-of-energy 3 300 power-grid))))
    (format NIL "~d,~d" (first largest-square) (second largest-square))))

(defun day11-1 ()
  (let* ((in (open "./day11.input"))
         (lst (read-by-line in))
         (result (solve-day11-1 (parse-integer (first lst)))))
    (close in)
    result))

(defun maximize-square-of-energy (grid-size power-grid)
  (let (largest-power largest-power-square)
    (loop :for size :from 1 :to grid-size
          :do (let* ((best (max-square-of-energy size grid-size power-grid))
                     (square (first best))
                     (power (second best)))
                (if (or (not largest-power)
                        (> power largest-power))
                  (setf largest-power-square square
                        largest-power power))))
    largest-power-square))

(defun solve-day11-2 (serial-number)
  (let* ((power-grid (create-power-grid 300 serial-number))
         (largest-square (maximize-square-of-energy 300 power-grid)))
    (format t "~S~%" largest-square)
    (format NIL "~d,~d,~d" (first largest-square) (second largest-square) (third largest-square))))

(defun day11-2 ()
  (solve-day11-2 1308))


(defun parse-pots (str)
  (let* ((parts (split-sequence:split-sequence #\Space str)))
    (third parts)))


(defun parse-rules (lst)
  (let* ((rules (make-hash-table :test 'equalp)))
    (loop for str in lst
          do (let* ((parts (split-sequence:split-sequence #\Space str)))
               (setf (gethash (first parts) rules) (third parts))))
    rules))


(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))


(defun grow-plants (center state rules)
  (let* ((leftmost-plant (position #\# state))
         (rightmost-plant (position #\# state :from-end T))
         (state-padded (concatenate
                         'string
                         "...."
                         (copy-seq (subseq state leftmost-plant (+ 1 rightmost-plant)))
                         "...."))
         (state-next (copy-seq state-padded)))
    (loop for i from 0 to (- (length state-padded) 5)
          do (let* ((window (subseq state-padded i (+ i 5))))
               (if (gethash window rules)
                 (setf (aref state-next (+ i 2))
                       (char (gethash window rules) 0)))))
    (list (+ (- center leftmost-plant) 4) state-next)))


(defun solve-day12-1 (center state rules generations)
  (loop for cur-gen from 1 to generations
        do (let* ((ret (grow-plants center state rules))
                  (center-next (first ret))
                  (state-next (second ret)))
             (setf center center-next
                   state state-next)
             (if (and
                   (> cur-gen 10000)
                   (= center center-next)
                   (string= state state-next))
               (progn
                 (setf center (- center (- generations cur-gen)))
                 (return)))))
  (loop for i from 0 to (- (length state) 1)
        when (char= (aref state i) #\#)
        summing (- i center)))


(defun day12-1 ()
  (let* ((in (open "./day12.input"))
         (lst (read-by-line in))
         (result (solve-day12-1 0
                                (parse-pots (first lst))
                                (parse-rules (rest lst))
                                20)))
    (close in)
    result))


(defun day12-2 ()
  (let* ((in (open "./day12.input"))
         (lst (read-by-line in))
         (result (solve-day12-1 0
                                (parse-pots (first lst))
                                (parse-rules (rest lst))
                                50000000000)))
    (close in)
    result))


(defun map-location (map x y)
  (aref (nth y map) x))


(defun next-vx (vx vy nturn nloc)
  (cond ((char= #\/ nloc) (cond ((not (= vx 0)) 0)
                                ((= vy 1)      -1)
                                ((= vy -1)      1)))
        ((char= #\\ nloc) (cond ((not (= vx 0)) 0)
                                ((= vy 1)       1)
                                ((= vy -1)     -1)))
        ((char= #\+ nloc) (cond ((string= "left"     nturn) (cond ((not (= vx 0)) 0)
                                                                  ((= vy 1)       1)
                                                                  ((= vy -1)     -1)))
                                ((string= "straight" nturn) vx)
                                ((string= "right"    nturn) (cond ((not (= vx 0)) 0)
                                                                  ((= vy 1)      -1)
                                                                  ((= vy -1)      1)))))
        (T vx)))


(defun next-vy (vx vy nturn nloc)
  (cond ((char= #\/ nloc) (cond ((not (= vy 0)) 0)
                                ((= vx 1)      -1)
                                ((= vx -1)      1)))
        ((char= #\\ nloc) (cond ((not (= vy 0)) 0)
                                ((= vx 1)       1)
                                ((= vx -1)     -1)))
        ((char= #\+ nloc) (cond ((string= "left"     nturn) (cond ((not (= vy 0)) 0)
                                                                  ((= vx 1)      -1)
                                                                  ((= vx -1)      1)))
                                ((string= "straight" nturn) vy)
                                ((string= "right"    nturn) (cond ((not (= vy 0)) 0)
                                                                  ((= vx 1)       1)
                                                                  ((= vx -1)     -1)))))
        (T vy)))


(defun next-turn (nturn nloc)
  (if (not (char= #\+ nloc))
    nturn
    (cond ((string= "left"     nturn) "straight")
          ((string= "straight" nturn) "right")
          ((string= "right"    nturn) "left"))))


(defun cart-collisions (carts)
  (let* ((occupied (make-hash-table :test 'equalp)))
    (loop for cart in carts
          do (let* ((pos (list (first cart) (second cart)))
                    (key (list-to-string pos))
                    (already-occupied (gethash key occupied)))
               (setf (gethash key occupied) (cons T already-occupied))))
    (loop for key in (hash-keys occupied)
          when (> (length (gethash key occupied)) 1)
          collect key)))


(defun parse-carts (lst)
  (let* ((cur-line)
         (c))
    (loop for y from 0 to (- (length lst) 1)
          do (setf cur-line (nth y lst))
          append (loop for x from 0 to (- (length cur-line) 1)
                       do (setf c (aref cur-line x))
                       when (or (char= #\^ c)
                                (char= #\v c)
                                (char= #\< c)
                                (char= #\> c))
                       collect (list
                                 x
                                 y
                                 (cond ((char= #\> c)  1)
                                       ((char= #\< c) -1)
                                       (T 0))
                                 (cond ((char= #\^ c) -1)
                                       ((char= #\v c)  1)
                                       (T 0))
                                 "left")))))


(defun destruction-derby (map carts &optional break-on-first-collition)
  (let* ((carts-by-position (make-hash-table :test 'equalp)))
    (loop for cart in carts
          do (let* ((pos (list (first cart) (second cart)))
                    (key (list-to-string pos)))
                (setf (gethash key carts-by-position)
                      (cons cart (gethash key carts-by-position)))))
    (loop for cart in carts
          when (gethash (list-to-string (list (first cart) (second cart))) carts-by-position)
          do (let* ((x (first cart))
                    (y (second cart))
                    (vx (third cart))
                    (vy (fourth cart))
                    (nturn (fifth cart))
                    (nx (+ x vx))
                    (ny (+ y vy))
                    (nloc (map-location map nx ny))
                    (ncart (list
                             nx
                             ny
                             (next-vx vx vy nturn nloc)
                             (next-vy vx vy nturn nloc)
                             (next-turn nturn nloc)))
                    (key (list-to-string (list x y)))
                    (nkey (list-to-string (list nx ny))))
               (remhash key carts-by-position)
               (setf (gethash nkey carts-by-position)
                     (cons ncart (gethash nkey carts-by-position)))
               (if (> (length (gethash nkey carts-by-position)) 1)
                 (if break-on-first-collition
                   (return)
                   (remhash nkey carts-by-position)))))
    (loop for key being the hash-keys of carts-by-position
          appending (gethash key carts-by-position))))


(defun move-carts-until-collision (map)
  (let* ((carts (parse-carts map))
         (carts-next (destruction-derby map carts T)))
    (loop while (null (cart-collisions carts-next))
          do (setf carts carts-next
                   carts-next (sort
                                (destruction-derby map carts T)
                                (lambda (c1 c2)
                                  (let* ((x1 (first c1))
                                         (y1 (second c1))
                                         (x2 (first c2))
                                         (y2 (second c2)))
                                    (or (< y1 y2)
                                        (and (= y1 y2)
                                             (< x1 x2))))))))
    (cart-collisions carts-next)))


(defun solve-day13-1 (map)
  (let* ((collisions (move-carts-until-collision map))
         (first-collision (first collisions))
         (parts (split-sequence:split-sequence #\Space first-collision)))
    (format nil "~d,~d" (first parts) (second parts))))


(defun day13-1 ()
  (let* ((in (open "./day13.input"))
         (lst (read-by-line in))
         (result (solve-day13-1 lst)))
    (close in)
    result))


(defun move-carts-until-one-car-left (map)
  (let* ((carts (parse-carts map))
         (carts-next (destruction-derby map carts)))
    (loop while (> (length carts-next) 1)
          do (setf carts carts-next
                   carts-next (sort
                                (destruction-derby map carts)
                                (lambda (c1 c2)
                                  (let* ((x1 (first c1))
                                         (y1 (second c1))
                                         (x2 (first c2))
                                         (y2 (second c2)))
                                    (or (< y1 y2)
                                        (and (= y1 y2)
                                             (< x1 x2))))))))
    (first carts-next)))


(defun solve-day13-2 (map)
  (let* ((cart (move-carts-until-one-car-left map)))
    (format nil "~d,~d" (first cart) (second cart))))


(defun day13-2 ()
  (let* ((in (open "./day13.input"))
         (lst (read-by-line in))
         (result (solve-day13-2 lst)))
    (close in)
    result))


(defun digits (number)
  (labels ((recurse (number)
             (if (< number 10)
               (list number)
               (cons (mod number 10) (recurse (floor number 10))))))
    (nreverse (recurse number))))


(defun generate-next-board (i j board)
  (let* ((first-score (aref board i))
         (second-score (aref board j))
         (next-score (+ first-score second-score)))
    (loop for d in (digits next-score)
          do (vector-push-extend d board))
    (list
      (mod (+ i first-score 1) (length board))
      (mod (+ j second-score 1) (length board)))))


(defun generate-first-board ()
    (make-array 2
                :fill-pointer 2
                :initial-contents '(3 7)))


(defun solve-day14-1 (start-looking)
  (let* ((board (generate-first-board))
         (state (generate-next-board 0 1 board))
         (ni (first state))
         (nj (second state)))
    (loop while (< (length board) (+ start-looking 10))
          do (setf state (generate-next-board ni nj board)
                   ni (first state)
                   nj (second state)))
    (format nil "~{~a~}" (coerce (subseq board start-looking (+ start-looking 10)) 'list))))


(defun day14-1 ()
  (solve-day14-1 190221))


(defun solve-day14-2 (scores-pattern)
  (let* ((board (generate-first-board))
         (state (generate-next-board 0 1 board))
         (ni (first state))
         (nj (second state))
         (offset 0))
    (loop do (progn
               (setf state (generate-next-board ni nj board)
                     ni (first state)
                     nj (second state))
               (if (>= (length board) (length scores-pattern))
                 (let* ((board-tail (subseq board offset (+ offset (length scores-pattern))))
                        (board-tail-str (format nil "~{~a~}" (coerce board-tail 'list))))
                   (if (string= scores-pattern board-tail-str)
                     (return offset)
                     (setf offset (+ offset 1)))))))))


(defun day14-2 ()
  (solve-day14-2 "190221"))


(defun parse-goblins-cave (x &optional (elf-attack 3))
  (let* ((height (length x))
         (width (length (first x)))
         (cave (make-array (list height width) :initial-contents x)))
    (loop :for y :from 0 :below (array-dimension cave 0)
          :do (loop :for x :from 0 :below (array-dimension cave 1)
                    :do (let ((cell (aref cave y x)))
                          (cond ((equal #\E cell) (setf (aref cave y x) (list cell elf-attack 200)))
                                ((equal #\G cell) (setf (aref cave y x) (list cell 3 200)))))))
    cave))

(defun print-goblins-cave (cave)
  (loop :for y :from 0 :below (array-dimension cave 0)
        :do (loop :for x :from 0 :below (array-dimension cave 1)
                  :do (let ((cell (aref cave y x)))
                        (format t "~c" (if (consp cell) (first cell) cell))))
        :do (loop :for x :from 0 :below (array-dimension cave 1)
                  :when (consp (aref cave y x))
                  :do (let ((cell (aref cave y x)))
                        (format t " ~c(~d)" (first cell) (third cell)))
                  :finally (format t "~%"))
        :finally (format t "~%")))

(defun cave-parse-unit (cave x y)
  (append (list x y) (aref cave y x)))

(defun cave-parse-units (cave)
  (loop :for y :from 0 :below (array-dimension cave 0)
        :appending (loop :for x :from 0 :below (array-dimension cave 1)
                         :when (consp (aref cave y x))
                        :collecting (cave-parse-unit cave x y))))

(defun cave-reading-order-p (p1 p2)
  (let ((x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2)))
    (or (< y1 y2)
        (and (= y1 y2)
             (< x1 x2)))))

(defun cave-unit-team (u)
  (third u))

(defun cave-adj-open-squares (cave curr)
  (let* ((x (first curr))
         (y (second curr))
         (adjs (list
                 (list x (- y 1))
                 (list (+ x 1) y)
                 (list x (+ y 1))
                 (list (- x 1) y))))
    (loop :for adj :in adjs
          :when (equal (aref cave (second adj) (first adj)) #\.)
          :collect adj)))

(defun cave-find-closest (cave u open-squares)
  (labels ((hash-key (curr)
             (format NIL "~d,~d" (first curr) (second curr))))
    (let ((frontier (list (list 0 (first u) (second u)))) ; priority x y
          (cost-so-far (make-hash-table :test 'equal)))
      (setf (gethash (hash-key u) cost-so-far) 0)
      ;; The following runs BFS, for **all** the map
      ;; XXX we could be smart and quit early as soon as all the open-squares 
      ;; have been visited
      (loop :while frontier
            :do (let* ((top (pop frontier))
                       (prio (first top))
                       (curr (rest top))
                       (prio-next (+ prio 1)))
                  (loop :for next :in (cave-adj-open-squares cave curr)
                        :do (let ((key (hash-key next)))
                              (unless (gethash key cost-so-far)
                                (let* ((item (cons prio-next next)))
                                  (setf (gethash key cost-so-far) prio-next
                                        frontier (cons item frontier)
                                        frontier (sort frontier #'< :key #'first))))))))
      (first
        (mapcar 
          #'second
          (sort 
            (mapcar #'(lambda (os) 
                        (list (gethash (hash-key os) cost-so-far) os))
                    (remove-if-not #'(lambda (os)
                                       (gethash (hash-key os) cost-so-far))
                                   open-squares))
            #'(lambda (wr1 wr2)
                (let ((d1 (first wr1))
                      (os1 (second wr1))
                      (d2 (first wr2))
                      (os2 (second wr2)))
                  (or (< d1 d2)
                      (and (= d1 d2)
                          (cave-reading-order-p os1 os2)))))))))))


(defun cave-find-enemies (cave u)
  (let* ((units (cave-parse-units cave)))
    (remove-if #'(lambda (uu)
                   (equal (cave-unit-team u)
                          (cave-unit-team uu)))
               units)))

(defun cave-find-move-target (cave u)
  (let* ((enemies (cave-find-enemies cave u))
         (open-squares (reduce
                         #'append
                         (mapcar
                           #'(lambda (target) (cave-adj-open-squares cave target))
                           enemies))))
    (cave-find-closest cave u open-squares)))

(defun cave-other-team-cell (u)
  (second
    (assoc
      (cave-unit-team u)
      '((#\E #\G)
        (#\G #\E)))))

(defun cave-adj-units (cave curr to-match)
  (let* ((x (first curr))
         (y (second curr))
         (adjs (list
                 (list x (- y 1))
                 (list (+ x 1) y)
                 (list x (+ y 1))
                 (list (- x 1) y))))
    (loop :for adj :in adjs
          :when (and (consp (aref cave (second adj) (first adj)))
                     (equal (first (aref cave (second adj) (first adj)))
                            to-match))
          :collect (append adj (aref cave (second adj) (first adj))))))

(defun cave-in-range (cave u)
  (cave-adj-units cave u (cave-other-team-cell u)))

(defun cave-move-unit (cave u target)
  ;; Bloody A-*
  (labels ((hash-key (curr)
             (format NIL "~d,~d" (first curr) (second curr)))
           (at-target (curr)
             (and (equal (first curr) (first u))
                  (equal (second curr) (second u))))
           (comes-first-p (e1 e2)
             (let ((prio-1 (first e1))
                   (pos-1 (rest e1))
                   (prio-2 (first e2))
                   (pos-2 (rest e2)))
               (or (< prio-1 prio-2)
                   (and (= prio-1 prio-2)
                        (cave-reading-order-p pos-1 pos-2))))))
    (setf (aref cave (second u) (first u)) #\.)
    (let ((frontier (list (list 0 (first target) (second target) 0))) ; priority x y steps
          (cost-so-far (make-hash-table :test 'equal))
          (come-from (make-hash-table :test 'equal)))
      (setf (gethash (hash-key target) cost-so-far) 0)
      (loop :while frontier
            :do (let* ((top (pop frontier))
                       (steps (fourth top))
                       (curr (rest top))
                       (prev (gethash (hash-key curr) come-from))
                       (steps-next (+ steps 1)))
                  (when (at-target curr)
                    (setf (aref cave (second prev) (first prev)) (nthcdr 2 u))
                    (return prev))
                  (loop :for next :in (cave-adj-open-squares cave curr)
                        :do (let* ((key (hash-key next)))
                              (when (and (gethash key cost-so-far)
                                         (comes-first-p (cons steps-next curr)
                                                        (cons
                                                          (gethash key cost-so-far)
                                                          (gethash key come-from))))
                                ;; simply update the prev pointer
                                ;; everythign else should be all right
                                (setf (gethash key come-from) curr))
                              (unless (gethash key cost-so-far)
                                (let* ((prio (+ steps-next
                                                (manhattan-distance-seq next u)))
                                       (item (append
                                               (list prio)
                                               next
                                               (list steps-next))))
                                  (setf (gethash key cost-so-far) steps-next
                                        (gethash key come-from) curr
                                        frontier (cons item frontier)
                                        frontier (sort frontier #'comes-first-p)))))))))))

(defun cave-unit-hits (u)
  (fifth u))

(defun cave-unit-attack (u)
  (fourth u))

(defun cave-find-attack-target (cave u)
  (let ((in-range-targets (sort
                            (cave-in-range cave u)
                            #'(lambda (u1 u2)
                                (let ((hits-1 (cave-unit-hits u1))
                                      (hits-2 (cave-unit-hits u2)))
                                  (or (< hits-1 hits-2)
                                      (and (= hits-1 hits-2)
                                            (cave-reading-order-p u1 u2))))))))
    (first in-range-targets)))


(defun cave-round (cave)
  (let* ((units (sort (cave-parse-units cave) #'cave-reading-order-p)))
    (loop :for u :in units
          :when (> (cave-unit-hits u) 0)
          :do (let ((moved u))
                (unless (cave-in-range cave u)
                  (unless (cave-find-enemies cave u)
                    (return 'incomplete))
                  (let* ((move-target (cave-find-move-target cave u))
                         (next NIL))
                    (when move-target
                      (setf next (cave-move-unit cave u move-target)
                            moved (cave-parse-unit cave (first next) (second next))))))
                (let ((attack-target (cave-find-attack-target cave moved)))
                  (when attack-target
                    (let* ((defense-new-hits (- (cave-unit-hits attack-target)
                                                (cave-unit-attack moved))))
                      (setf (fifth attack-target) defense-new-hits)
                      (if (<= defense-new-hits 0)
                        (setf (aref cave (second attack-target) (first attack-target)) #\.)))))))))

(defun cave-same-team (units)
  (every #'(lambda (u) (equal (cave-unit-team u)
                              (cave-unit-team (first units))))
         units))

(defun solve-day15-1 (cave &optional debug)
  (if debug (print-goblins-cave cave))
  (do ((n 1 (+ n 1)))
      (NIL)
    (when (equal (cave-round cave) 'incomplete)
      (when debug
        (format t "Round: ~S (incomplete)~%" n)
        (print-goblins-cave cave))
      (let* ((completed-rounds (- n 1))
             (units (cave-parse-units cave))
             (hits (reduce #'+ (mapcar #'cave-unit-hits units))))
        (if debug (format t "Rounds: ~S Hits: ~S~%" completed-rounds hits))
        (return (* completed-rounds hits))))
    (when debug
      (format t "Round: ~S~%" n)
      (print-goblins-cave cave))))

(defun day15-1 ()
  (let* ((in (open "./day15.input"))
         (lst (read-by-line in))
         (result (solve-day15-1 (parse-goblins-cave lst))))
    (close in)
    result))

(defun cave-count-elf-units (cave)
  (length
    (remove-if-not #'(lambda (u) (equal (cave-unit-team u) #\E))
                   (cave-parse-units cave))))

(defun solve-day15-2 (x)
  (do ((attack-power 4 (+ attack-power 1)))
      (NIL)
    (let* ((cave (parse-goblins-cave x attack-power))
           (starting-units (cave-count-elf-units cave))
           (outcome (solve-day15-1 cave))
           (ending-units (cave-count-elf-units cave)))
      (if (= starting-units ending-units)
        (return outcome)))))

(defun day15-2 ()
  (let* ((in (open "./day15.input"))
         (lst (read-by-line in))
         (result (solve-day15-2 lst)))
    (close in)
    result))

(defun replace-nth (n value x)
  (if (= n 0)
    (cons value (rest x))
    (cons (first x) (replace-nth (- n 1) value (rest x)))))


(defun opcode-addr (registers args)
  (replace-nth (third args)
               (+ (nth (first args) registers)
                  (nth (second args) registers))
               registers))

(defun opcode-addi (registers args)
  (replace-nth (third args)
               (+ (nth (first args) registers)
                  (second args))
               registers))

(defun opcode-mulr (registers args)
  (replace-nth (third args)
               (* (nth (first args) registers)
                  (nth (second args) registers))
               registers))

(defun opcode-muli (registers args)
  (replace-nth (third args)
               (* (nth (first args) registers)
                  (second args))
               registers))

(defun opcode-banr (registers args)
  (replace-nth (third args)
               (logand (nth (first args) registers)
                       (nth (second args) registers))
               registers))

(defun opcode-bani (registers args)
  (replace-nth (third args)
               (logand (nth (first args) registers)
                       (second args))
               registers))

(defun opcode-borr (registers args)
  (replace-nth (third args)
               (logior (nth (first args) registers)
                       (nth (second args) registers))
               registers))

(defun opcode-bori (registers args)
  (replace-nth (third args)
               (logior (nth (first args) registers)
                       (second args))
               registers))

(defun opcode-setr (registers args)
  (replace-nth (third args)
               (nth (first args) registers)
               registers))

(defun opcode-seti (registers args)
  (replace-nth (third args)
               (first args)
               registers))

(defun opcode-gtir (registers args)
  (replace-nth (third args)
               (if (> (first args) (nth (second args) registers))
                 1
                 0)
               registers))

(defun opcode-gtri (registers args)
  (replace-nth (third args)
               (if (> (nth (first args) registers) (second args))
                 1
                 0)
               registers))

(defun opcode-gtrr (registers args)
  (replace-nth (third args)
               (if (> (nth (first args) registers) (nth (second args) registers))
                 1
                 0)
               registers))

(defun opcode-eqir (registers args)
  (replace-nth (third args)
               (if (= (first args) (nth (second args) registers))
                 1
                 0)
               registers))

(defun opcode-eqri (registers args)
  (replace-nth (third args)
               (if (= (nth (first args) registers) (second args))
                 1
                 0)
               registers))

(defun opcode-eqrr (registers args)
  (replace-nth (third args)
               (if (= (nth (first args) registers) (nth (second args) registers))
                 1
                 0)
               registers))

(defun opcodes ()
  (list
    #'opcode-addr
    #'opcode-addi
    #'opcode-mulr
    #'opcode-muli
    #'opcode-banr
    #'opcode-bani
    #'opcode-borr
    #'opcode-bori
    #'opcode-setr
    #'opcode-seti
    #'opcode-gtir
    #'opcode-gtri
    #'opcode-gtrr
    #'opcode-eqir
    #'opcode-eqri
    #'opcode-eqrr))

(defun opcodes-matching (registers-before instruction registers-after)
  (let* ((args (rest instruction))
         (output-reg (third args)))
    (loop for opcode in (opcodes)
          when (= (nth output-reg registers-after)
                  (nth output-reg (funcall opcode registers-before args)))
          collect opcode)))

(defun parse-numbers (separator str)
  (let* ((numbers (split-sequence:split-sequence separator str)))
    (mapcar (lambda (x) (parse-integer x :junk-allowed t)) numbers)))

(defun parse-register (str)
  (let* ((parts (split-sequence:split-sequence #\[ str)))
    (parse-numbers #\, (second parts))))

(defun solve-day16-1 (lst)
  (length
    (loop for inputs in lst
          when (>= (length (apply 'opcodes-matching inputs)) 3)
          collect inputs)))

(defun day16-1 ()
  (let* ((in (open "./day16.input"))
         (lst (read-by-line in))
         (result (solve-day16-1 (loop for a in lst by #'cddddr
                                      for b in (cdr lst) by #'cddddr
                                      for c in (cddr lst) by #'cddddr
                                      for d in (cdddr lst) by #'cddddr
                                      collect (list
                                                (parse-register a)
                                                (parse-numbers #\Space b)
                                                (parse-register c))))))

    (close in)
    result))


(defun make-opcodes-table (lst)
  (let* ((opcodes-table ()))
    (labels ((recurse ()
               (loop for inputs in lst
                     when (not (assoc (caadr inputs) opcodes-table))
                     do (let* ((matched (remove-if (lambda (opcode) (rassoc opcode opcodes-table))
                                                   (apply 'opcodes-matching inputs))))
                          (if (= (length matched) 1)
                            (push (cons (caadr inputs) (first matched))
                                  opcodes-table))))
               (if (< (length opcodes-table) 16)
                 (recurse)
                 opcodes-table)))
      (recurse))))


(defun solve-day16-2 (lst test-program)
  (let* ((opcodes-table (make-opcodes-table lst))
         (registers (list 0 0 0 0)))
    (loop for instruction in test-program
          do (let* ((opcode-id (first instruction))
                    (args (rest instruction))
                    (opcode (rest (assoc opcode-id opcodes-table))))
               (setf registers (funcall opcode registers args))))
    (first registers)))


(defun day16-2 ()
  (let* ((in (open "./day16.input"))
         (lst (read-by-line in))
         (in2 (open "./day16b.input"))
         (lst2 (read-by-line in2))
         (result (solve-day16-2 (loop for a in lst by #'cddddr
                                      for b in (cdr lst) by #'cddddr
                                      for c in (cddr lst) by #'cddddr
                                      for d in (cdddr lst) by #'cddddr
                                      collect (list
                                                (parse-register a)
                                                (parse-numbers #\Space b)
                                                (parse-register c)))
                                (mapcar
                                  (lambda (x) (parse-numbers #\Space x))
                                  lst2))))
    (close in2)
    (close in)
    result))


(defun parse-clay-line (str)
  (let* ((parts (split-sequence:split-sequence #\, str))
         (parts-1 (split-sequence:split-sequence #\= (first parts)))
         (x-p (string= "x" (first parts-1)))
         (value (parse-integer (second parts-1) :junk-allowed T))
         (parts-2 (split-sequence:split-sequence #\= (second parts)))
         (range-numbers (split-sequence:split-sequence #\. (second parts-2)))
         (range-value-1 (parse-integer (first range-numbers) :junk-allowed T))
         (range-value-2 (parse-integer (third range-numbers) :junk-allowed T)))
   (if x-p
     (list (list value range-value-1) (list value range-value-2))
     (list (list range-value-1 value) (list range-value-2 value)))))

(defun parse-ground (clay-lines)
  (let* ((points (cons '(500 0) (reduce #'append clay-lines)))
         (xs (mapcar #'first points))
         (ys (mapcar #'second points))
         (minx (apply 'min xs))
         (maxx (apply 'max xs))
         (miny (apply 'min ys))
         (maxy (apply 'max ys))
         (sizex (+ 3 (- maxx minx)))
         (sizey (+ 1 (- maxy miny)))
         (ground (make-array (list sizey sizex) :initial-element #\.)))
    (setf (aref ground 0 (+ 1 (- 500 minx))) #\+)
    (loop :for line :in clay-lines
          :do (loop :for x :from (first (first line)) :to (first (second line))
                    :do (loop :for y :from (second (first line)) :to (second (second line))
                              :do (setf (aref ground (- y miny) (+ 1 (- x minx))) #\#))))
    ground))

(defun obstacles()
    (list #\# #\~))

(defun can-go-down-p (y x ground)
  (and
    (< (+ y 1) (array-dimension ground 0))
    (not (member (aref ground (+ y 1) x) (obstacles)))))

(defun can-go-left-p (y x ground)
  (and
    (not (can-go-down-p y x ground))
    (> x 0)
    (not (member (aref ground y (- x 1)) (obstacles)))))

(defun can-go-right-p (y x ground)
  (and
    (not (can-go-down-p y x ground))
    (< (+ x 1) (array-dimension ground 1))
    (not (member (aref ground y (+ x 1)) (obstacles)))))

(defun unrest-water-left (y x ground)
  (if (> x 0)
    (when (char= (aref ground y (- x 1)) #\~)
      (setf (aref ground y (- x 1)) #\|)
      (unrest-water-left y (- x 1) ground))))

(defun unrest-water-right (y x ground)
  (if (< (+ x 1) (array-dimension ground 1))
    (when (char= (aref ground y (+ x 1)) #\~)
      (setf (aref ground y (+ x 1)) #\|)
      (unrest-water-right y (+ x 1) ground))))

(defun go-down (y x ground)
  (if (or (= (+ y 1) (array-dimension ground 0))
          (char= (aref ground (+ y 1) x) #\|)
          (and (not (char= (aref ground (+ y 1) x) #\.))
               (or (and (> x 0)
                        (char= (aref ground y (- x 1)) #\|))
                   (and (< (+ x 1) (array-dimension ground 1))
                        (char= (aref ground y (+ x 1)) #\|)))))
    (progn
      (unrest-water-left y x ground)
      (unrest-water-right y x ground)
      (setf (aref ground y x) #\|))
    (let* ((down-ok (can-go-down-p y x ground))
           (left-ok (can-go-left-p y x ground))
           (right-ok (can-go-right-p y x ground)))
      (if down-ok (go-down (+ y 1) x ground))
      (if left-ok (go-left y (- x 1) ground))
      (if right-ok (go-right y (+ x 1) ground))
      (if (or down-ok left-ok right-ok)
        (go-down y x ground)
        (progn
          (setf (aref ground y x) #\~))))))


(defun go-left (y x ground)
  (if (or (and (= x 0)
               (and (< (+ y 1) (array-dimension ground 0))
                    (member (aref ground (+ y 1) x) (obstacles))))
          (and (< (+ y 1) (array-dimension ground 0))
               (char= (aref ground (+ y 1) x) #\|))
          (and (> x 0)
               (char= (aref ground y (- x 1)) #\|)
               (and (< (+ y 1) (array-dimension ground 0))
                    (member (aref ground (+ y 1) x) (obstacles)))))
    (progn
      (setf (aref ground y x) #\|))
    (let* ((down-ok (can-go-down-p y x ground))
           (left-ok (can-go-left-p y x ground)))
      (if down-ok (go-down (+ y 1) x ground))
      (if left-ok (go-left y (- x 1) ground))
      (if (or down-ok left-ok)
        (go-left y x ground)
        (progn
          (setf (aref ground y x) #\~))))))

(defun go-right (y x ground)
  (if (or (and (= (+ x 1) (array-dimension ground 1))
               (and (< (+ y 1) (array-dimension ground 0))
                    (member (aref ground (+ y 1) x) (obstacles))))
          (and (< (+ y 1) (array-dimension ground 0))
               (char= (aref ground (+ y 1) x) #\|))
          (and (< (+ x 1) (array-dimension ground 1))
               (char= (aref ground y (+ x 1)) #\|)
               (and (< (+ y 1) (array-dimension ground 0))
                    (member (aref ground (+ y 1) x) (obstacles)))))
    (progn
      (setf (aref ground y x) #\|))
    (let* ((down-ok (can-go-down-p y x ground))
           (right-ok (can-go-right-p y x ground)))
      (if down-ok (go-down (+ y 1) x ground))
      (if right-ok (go-right y (+ x 1) ground))
      (if (or down-ok right-ok)
        (go-right y x ground)
        (progn
          (setf (aref ground y x) #\~))))))


(defun print-vector (vector)
  (loop for y from 0 below (array-dimension vector 0)
        do (loop for x from 0 below (array-dimension vector 1)
                 do (format t "~c" (aref vector y x))
                 finally (format t "~%"))
        finally (format t "~%")))


(defun find-spring (ground)
  (first
    (loop :for x :from 0 :below (array-dimension ground 1)
          :when (char= (aref ground 0 x) #\+)
          :collect (list 0 x))))

(defun first-clay-y (ground)
  (block outer ;; hugly!
    (loop :for y :from 0 :below (array-dimension ground 0)
          :do (loop :for x :from 0 :below (array-dimension ground 1)
                    :when (char= (aref ground y x) #\#)
                    :do (return-from outer y)))))

(defun count-water (ground water-types)
  (let* ((first-y (first-clay-y ground)))
    (loop :for y :from first-y :below (array-dimension ground 0)
          :summing (loop :for x :from 0 :below (array-dimension ground 1)
                        :when (member (aref ground y x) water-types)
                        :summing 1))))

(defun solve-day17-1 (ground)
  (let* ((spring (find-spring ground))
         (spring-y (first spring))
         (spring-x (second spring)))
    (go-down (+ 1 spring-y) spring-x ground)
    (count-water ground '(#\| #\~))))

(defun day17-1 ()
  (let* ((in (open "./day17.input"))
         (lst (read-by-line in))
         (result (solve-day17-1 (parse-ground (mapcar #'parse-clay-line lst)))))
    (close in)
    result))

(defun solve-day17-2 (ground)
  (let* ((spring (find-spring ground))
         (spring-y (first spring))
         (spring-x (second spring)))
    (go-down (+ 1 spring-y) spring-x ground)
    (count-water ground '(#\~))))

(defun day17-2 ()
  (let* ((in (open "./day17.input"))
         (lst (read-by-line in))
         (result (solve-day17-2 (parse-ground (mapcar #'parse-clay-line lst)))))
    (close in)
    result))


(defun parse-lumber-area (lst)
  (make-array (list (length lst) (length (first lst))) :initial-contents lst))

(defun adjacents (y x lumber-area)
  (loop :for yy :from (- y 1) :to (+ y 1)
        :when (and (>= yy 0) (< yy (array-dimension lumber-area 0)))
        :append (loop :for xx :from (- x 1) :to (+ x 1)
                      :when (and
                              (>= xx 0)
                              (< xx (array-dimension lumber-area 1))
                              (not (and (= x xx) (= y yy))))
                      :collect (aref lumber-area yy xx))))

(defun acre-open-p (x) (char= x #\.))
(defun acre-tree-p (x) (char= x #\|))
(defun acre-lumberyard-p (x) (char= x #\#))

(defun transform-acre (curr surrounding)
  (let* ((surrounding-trees (remove-if-not #'acre-tree-p surrounding))
         (surrounding-lumberyards (remove-if-not #'acre-lumberyard-p surrounding)))
    (cond
      ((acre-open-p curr) (if (>= (length surrounding-trees) 3) #\| curr))
      ((acre-tree-p curr) (if (>= (length surrounding-lumberyards) 3) #\# curr))
      ((acre-lumberyard-p curr) (if (and (>= (length surrounding-lumberyards) 1)
                                         (>= (length surrounding-trees) 1)) curr #\.)))))

(defun transform-lumber-area (lumber-area)
  (let* ((lumber-area-next (make-array (array-dimensions lumber-area))))
    (loop :for y :from 0 :below (array-dimension lumber-area 0)
          :do (loop :for x :from 0 :below (array-dimension lumber-area 1)
                    :do (setf (aref lumber-area-next y x)
                              (transform-acre (aref lumber-area y x)
                                              (adjacents y x lumber-area)))))
   lumber-area-next))

(defun vector-to-string (vector)
  (concatenate
    'string
    (loop :for y :from 0 :below (array-dimension vector 0)
          :append (loop :for x :from 0 :below (array-dimension vector 1)
                        :collect (aref vector y x)))))

(defun vector-collect (vector &key test)
  (loop :for y :from 0 :below (array-dimension vector 0)
        :append (loop :for x :from 0 :below (array-dimension vector 1)
                      :when (funcall test (aref vector y x))
                      :collect (aref vector y x))))

(defun resource-value (lumber-area-str)
  (*
    (loop :for c :across lumber-area-str :when (acre-tree-p c) :summing 1)
    (loop :for c :across lumber-area-str :when (acre-lumberyard-p c) :summing 1)))

(defun solve-day18-1 (lumber-area minutes)
  (let* ((memo NIL)
         (last-run (loop :for m :from 1 :to minutes
                         :do (let* ((key (vector-to-string lumber-area)))
                               (if (member key memo :test 'equal)
                                 (return (- m 1))
                                 (setf lumber-area (transform-lumber-area lumber-area)
                                       memo (cons key memo))))
                         :finally (return (- m 1)))))
    (if (= last-run minutes)
      (resource-value (vector-to-string lumber-area))
      (let* ((key (vector-to-string lumber-area))
             (looop (member key (reverse memo) :test 'equal))
             (remaining (- minutes last-run))
             (index (mod remaining (length looop)))
             (last (nth index looop)))
        (resource-value last)))))


(defun day18-1 ()
  (let* ((in (open "./day18.input"))
         (lst (read-by-line in))
         (result (solve-day18-1 (parse-lumber-area lst)
                                10)))
    (close in)
    result))

(defun day18-2 ()
  (let* ((in (open "./day18.input"))
         (lst (read-by-line in))
         (result (solve-day18-1 (parse-lumber-area lst)
                                1000000000)))
    (close in)
    result))

(defun parse-instruction-pointer (str)
  (parse-integer (second (split-sequence:split-sequence #\Space str))))


(defparameter *opcodes-by-name* NIL)
(setf *opcodes-by-name* (list
                          (list "addr" #'opcode-addr)
                          (list "addi" #'opcode-addi)
                          (list "mulr" #'opcode-mulr)
                          (list "muli" #'opcode-muli)
                          (list "banr" #'opcode-banr)
                          (list "bani" #'opcode-bani)
                          (list "borr" #'opcode-borr)
                          (list "bori" #'opcode-bori)
                          (list "setr" #'opcode-setr)
                          (list "seti" #'opcode-seti)
                          (list "gtir" #'opcode-gtir)
                          (list "gtri" #'opcode-gtri)
                          (list "gtrr" #'opcode-gtrr)
                          (list "eqir" #'opcode-eqir)
                          (list "eqri" #'opcode-eqri)
                          (list "eqrr" #'opcode-eqrr)))

(defun parse-instruction (str)
  (let* ((parts (split-sequence:split-sequence #\Space str))
         (fn (second (assoc (first parts) *opcodes-by-name* :test 'equal)))
         (args (mapcar #'parse-integer (rest parts))))
    (list fn args)))

(defun divisors (n)
  (append
    (loop :for d :from 1 :to (floor n 2)
          :when (= 0 (mod n d))
          :collect d)
    (list n)))

(defun solve-day19-1 (ip-reg instructions registers-init)
  (let* ((ip-for-number (if (= (nth 0 registers-init) 0) 24 33))
         (registers (copy-seq registers-init))
         (n (loop :while (< (nth ip-reg registers) (length instructions))
                  :do (let* ((instruction (nth (nth ip-reg registers) instructions))
                             (fn (first instruction))
                             (args (second instruction))
                             (registers-after (funcall fn registers args)))
                        (if (= (nth ip-reg registers) ip-for-number)
                          (return (nth 2 registers-after)))
                        (setf registers (replace-nth ip-reg (+ (nth ip-reg registers-after) 1) registers-after))))))
    (apply #'+ (divisors n))))

(defun day19-1 ()
  (let* ((in (open "./day19.input"))
         (lst (read-by-line in))
         (result (solve-day19-1 (parse-instruction-pointer (first lst))
                                (mapcar #'parse-instruction (rest lst))
                                (list 0 0 0 0 0 0))))
    (close in)
    result))

(defun day19-2 ()
  (let* ((in (open "./day19.input"))
         (lst (read-by-line in))
         (result (solve-day19-1 (parse-instruction-pointer (first lst))
                                (mapcar #'parse-instruction (rest lst))
                                (list 1 0 0 0 0 0))))
    (close in)
    result))

(defun parse-facility-map (str)
  (let* ((lst (coerce str 'list))
         (remaining (subseq lst 1 (- (length lst) 1)))
         (branches NIL)
         (curr-x 0)
         (curr-y 0)
         (facility-map (make-hash-table :test 'equal)))
    (loop :while remaining
          :do (let ((head (pop remaining)))
                (cond ((equal #\N head)
                       (setf (gethash (format NIL "~d,~d" curr-x (- curr-y 1)) facility-map) #\-
                             (gethash (format NIL "~d,~d" curr-x (- curr-y 2)) facility-map) #\.
                             curr-y (- curr-y 2)))
                      ((equal #\E head)
                       (setf (gethash (format NIL "~d,~d" (+ curr-x 1) curr-y) facility-map) #\|
                             (gethash (format NIL "~d,~d" (+ curr-x 2) curr-y) facility-map) #\.
                             curr-x (+ curr-x 2)))
                      ((equal #\S head)
                       (setf (gethash (format NIL "~d,~d" curr-x (+ curr-y 1)) facility-map) #\-
                             (gethash (format NIL "~d,~d" curr-x (+ curr-y 2)) facility-map) #\.
                             curr-y (+ curr-y 2)))
                      ((equal #\W head)
                       (setf (gethash (format NIL "~d,~d" (- curr-x 1) curr-y) facility-map) #\|
                             (gethash (format NIL "~d,~d" (- curr-x 2) curr-y) facility-map) #\.
                             curr-x (- curr-x 2)))
                      ((equal #\( head)
                       (setf branches (cons (list curr-x curr-y) branches)))
                      ((equal #\| head)
                       (setf curr-x (first (first branches))
                             curr-y (second (first branches))))
                      ((equal #\) head)
                       (setf curr-x (first (first branches))
                             curr-y (second (first branches))
                             branches (rest branches) )))))
    facility-map))

(defun facility-adjacent-rooms (curr facility-map)
  (let ((x (first curr))
        (y (second curr))
        (cost (+ (third curr) 1))
        adj)
    (if (equal #\- (gethash (format NIL "~d,~d" x (- y 1)) facility-map))
      (push (list x (- y 2) cost) adj))
    (if (equal #\| (gethash (format NIL "~d,~d" (+ x 1) y) facility-map))
      (push (list (+ x 2) y cost) adj))
    (if (equal #\- (gethash (format NIL "~d,~d" x (+ y 1)) facility-map))
      (push (list x (+ y 2) cost) adj))
    (if (equal #\| (gethash (format NIL "~d,~d" (- x 1) y) facility-map))
      (push (list (- x 2) y cost) adj))
    adj))

(defun calculate-doors-to-room (facility-map)
  (let ((frontier (list '(0 0 0 0))) ; priority x y cost
        (cost-so-far (make-hash-table :test 'equal))
        (come-from (make-hash-table :test 'equal)))
    (setf (gethash "0,0" cost-so-far) 0)
    (loop :while frontier
          :do (let* ((curr (rest (pop frontier))))
                (loop :for next :in (facility-adjacent-rooms curr facility-map)
                      :do (let ((key (format NIL "~d,~d" (first next) (second next))))
                            (when (or (null (gethash key cost-so-far))
                                      (< (third next)
                                         (gethash key cost-so-far)))
                              (let* ((item (cons (third next) next)))
                                (setf (gethash key cost-so-far) (third next)
                                      (gethash key come-from) curr
                                      frontier (cons item frontier)
                                      frontier (sort frontier #'< :key #'first))))))))
    cost-so-far))


(defun solve-day20-1 (str)
  (let* ((facility-map (parse-facility-map str))
         (doors-to-room (calculate-doors-to-room facility-map)))
    (loop :for key :being :the :hash-keys :of doors-to-room
          :maximizing (gethash key doors-to-room))))

(defun day20-1 ()
  (let* ((in (open "./day20.input"))
         (lst (read-by-line in))
         (result (solve-day20-1 (first lst))))
    (close in)
    result))

(defun solve-day20-2 (str)
  (let* ((facility-map (parse-facility-map str))
         (doors-to-room (calculate-doors-to-room facility-map)))
    (loop :for key :being :the :hash-keys :of doors-to-room
          :when (>= (gethash key doors-to-room) 1000)
          :summing 1)))

(defun day20-2 ()
  (let* ((in (open "./day20.input"))
         (lst (read-by-line in))
         (result (solve-day20-2 (first lst))))
    (close in)
    result))

(defun solve-day21-1 (ip-reg instructions registers-init)
  (let* ((registers (copy-seq registers-init)))
    (loop :while (< (nth ip-reg registers) (length instructions))
          :do (let* ((instruction (nth (nth ip-reg registers) instructions))
                     (fn (first instruction))
                     (args (second instruction))
                     (registers-after (funcall fn registers args)))
                (if (= (nth ip-reg registers) 28)
                  (return (nth (first args) registers)))
                (setf registers (replace-nth ip-reg (+ (nth ip-reg registers-after) 1) registers-after))))))

(defun day21-1 ()
  (let* ((in (open "./day21.input"))
         (lst (read-by-line in))
         (result (solve-day21-1 (parse-instruction-pointer (first lst))
                                (mapcar #'parse-instruction (rest lst))
                                (list 0 0 0 0 0 0))))
    (close in)
    result))

(defun solve-day21-2 (ip-reg instructions registers-init)
  (let* ((registers (copy-seq registers-init))
         (seen (make-hash-table))
         prev)
    (loop :while (< (nth ip-reg registers) (length instructions))
          :do (let* ((instruction (nth (nth ip-reg registers) instructions))
                     (fn (first instruction))
                     (args (second instruction))
                     (registers-after (funcall fn registers args)))
                (when (= (nth ip-reg registers) 28)
                  (let ((check (nth (first args) registers)))
                    (if (gethash check seen)
                      (return prev)
                      (setf prev check
                            (gethash check seen) T))))
                (setf registers (replace-nth ip-reg (+ (nth ip-reg registers-after) 1) registers-after))))))

(defun day21-2 ()
  (let* ((in (open "./day21.input"))
         (lst (read-by-line in))
         (result (solve-day21-2 (parse-instruction-pointer (first lst))
                                (mapcar #'parse-instruction (rest lst))
                                (list 0 0 0 0 0 0))))
    (close in)
    result))

(defun parse-cave-depth (str)
  (let* ((parts (split-sequence:split-sequence #\Space str)))
    (parse-integer (second parts) :junk-allowed T)))

(defun parse-cave-target (str)
  (let* ((parts (split-sequence:split-sequence #\Space str))
         (coords (split-sequence:split-sequence #\, (second parts))))
    (list
      (parse-integer (first coords) :junk-allowed T)
      (parse-integer (second coords) :junk-allowed T))))

(defun geologic-index (x y target depth)
  "The geologic index can be determined using the first rule that applies from the list below:

  The region at 0,0 (the mouth of the cave) has a geologic index of 0.
  The region at the coordinates of the target has a geologic index of 0.
  If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.
  If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.
  Otherwise, the region's geologic index is the result of multiplying the erosion levels of the regions at X-1,Y and X,Y-1."
  (cond ((and (zerop x) (zerop y)) 0)
        ((and (equal x (first target))
              (equal y (second target))) 0)
        ((zerop y) (* x 16807))
        ((zerop x) (* y 48271))
        (T (* (erosion-level (- x 1) y target depth)
              (erosion-level x (- y 1) target depth)))))

(defvar *geologic-index-cache*)

(defun geologic-index-cached (x y target depth)
  (let* ((key (format nil "~d,~d" x y)))
    (if (not (gethash key *geologic-index-cache*))
      (setf (gethash key *geologic-index-cache*) (geologic-index x y target depth)))
    (gethash key *geologic-index-cache*)))

(defun erosion-level (x y target depth)
  "A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183."
  (mod (+ (geologic-index-cached x y target depth) depth) 20183))

(defun area-type (x y target depth)
  (let* ((el (erosion-level x y target depth)))
    (mod el 3)))

(defun solve-day22-1 (depth target)
  (setf *geologic-index-cache* (make-hash-table :test 'equal))
  (loop :for x :from 0 :to (first target)
        :summing (loop :for y :from 0 :to (second target)
                       :summing (area-type x y target depth))))

(defun day22-1 ()
  (let* ((in (open "./day22.input"))
         (lst (read-by-line in))
         (result (solve-day22-1 (parse-cave-depth (first lst))
                                (parse-cave-target (second lst)))))
    (close in)
    result))

(defvar *required-tools-by-area-type* '((0 climbing-gear torch)
                                        (1 climbing-gear none)
                                        (2 torch none)))

(defvar *next-tools-by-area-type* '((0 ((climbing-gear torch)
                                        (torch climbing-gear)))
                                    (1 ((climbing-gear none)
                                        (none climbing-gear)))
                                    (2 ((torch none)
                                        (none torch)))))

(defun cave-adjacents (x y)
  (let (adj)
    (if (> y 0)
      (push (list x (- y 1)) adj))
    (push (list (+ x 1) y) adj)
    (push (list x (+ y 1)) adj)
    (if (> x 0)
      (push (list (- x 1) y) adj) adj)
    adj))

(defun cave-next-tools (atype tools)
  (let ((transformation (second (assoc atype *next-tools-by-area-type*))))
    (second (assoc tools transformation))))

(defun cave-next-steps (curr target depth)
  (let* ((x (first curr))
         (y (second curr))
         (tools (third curr))
         (time (fourth curr))
         (atype (area-type x y target depth)))
    (cons
      (list x y (cave-next-tools atype tools) (+ time 7))
      (loop :for adj :in (cave-adjacents x y)
            :appending (let* ((nx (first adj))
                              (ny (second adj))
                              (natype (area-type nx ny target depth))
                              (ntools-choices (rest (assoc natype *required-tools-by-area-type*))))
                         (if (member tools ntools-choices)
                           (list (list nx ny tools (+ time 1)))))))))

(defun cave-backtrack (curr come-from)
  (let ((key (format NIL "~d,~d,~a" (first curr) (second curr) (fourth curr))))
    (cond ((not (gethash key come-from)) (list curr))
          (T (cons curr (cave-backtrack (gethash key come-from) come-from))))))

(defun solve-day22-2 (depth target)
  (setf *geologic-index-cache* (make-hash-table :test 'equal))
  (let ((frontier (list '(0 0 0 torch 0))) ; priority x y tools time
        (cost-so-far (make-hash-table :test 'equal))
        (come-from (make-hash-table :test 'equal)))
    (setf (gethash "0,0,TORCH" cost-so-far) 0)
    (loop :while frontier
          ; :do (format t "~A~%" (rest (first frontier)))
          :do (let* ((curr (rest (pop frontier)))
                     (at-target (and (equal (first curr) (first target))
                                     (equal (second curr) (second target))
                                     (equal (third curr) 'torch))))
                (if at-target
                  (return (fourth curr))
                  (loop :for next :in (cave-next-steps curr target depth)
                        :do (let ((key (format NIL "~d,~d,~a" (first next) (second next) (third next))))
                              (when (or (null (gethash key cost-so-far))
                                        (< (fourth next)
                                           (gethash key cost-so-far)))
                                (let* ((priority (+ (fourth next)
                                                    (manhattan-distance-seq next target)))
                                       (item (cons priority next)))
                                  (setf (gethash key cost-so-far) (fourth next)
                                        (gethash key come-from) curr
                                        frontier (cons item frontier)
                                        frontier (sort frontier #'< :key #'first)))))))))))

(defun day22-2 ()
  (let* ((in (open "./day22.input"))
         (lst (read-by-line in))
         (result (solve-day22-2 (parse-cave-depth (first lst))
                                (parse-cave-target (second lst)))))
    (close in)
    result))

(defun parse-nanobot (str)
  (let* ((parts (split-sequence:split-sequence #\< str))
         (coords (split-sequence:split-sequence #\, (second parts)))
         (x (parse-integer (first coords) :junk-allowed T))
         (y (parse-integer (second coords) :junk-allowed T))
         (z (parse-integer (third coords) :junk-allowed T))
         (parts (split-sequence:split-sequence #\= str))
         (r (parse-integer (third parts) :junk-allowed T)))
    (list x y z r)))

(defun manhattan-distance-seq (seq1 seq2)
  (reduce #'+ (mapcar #'abs (mapcar #'- seq1 seq2))))

(defun solve-day23-1 (nanobots)
  (let* ((sorted (sort (copy-seq nanobots) #'> :key #'fourth))
         (stronger (first sorted)))
    (length
      (remove-if
        #'(lambda (n) (> (manhattan-distance-seq (subseq stronger 0 3)
                                                 (subseq n 0 3))
                         (fourth stronger)))
        nanobots))))

(defun day23-1 ()
  (let* ((in (open "./day23.input"))
         (lst (read-by-line in))
         (result (solve-day23-1 (mapcar #'parse-nanobot lst))))
    (close in)
    result))

(defun 3d-grid (start end step)
  (loop :for x :from (first start) :to (first end) :by step
        :appending (loop :for y :from (second start) :to (second end) :by step
                         :appending (loop :for z :from (third start) :to (third end) :by step
                                          :collecting (list x y z)))))

(defun nanobots-in-range (p nanobots)
  (remove-if #'(lambda (n) (> (manhattan-distance-seq p n)
                              (fourth n)))
             nanobots))

(defun solve-day23-2 (nanobots)
  (let* ((min-x (apply #'min (mapcar #'first nanobots)))
         (min-y (apply #'min (mapcar #'second nanobots)))
         (min-z (apply #'min (mapcar #'third nanobots)))
         (max-x (apply #'max (mapcar #'first nanobots)))
         (max-y (apply #'max (mapcar #'second nanobots)))
         (max-z (apply #'max (mapcar #'third nanobots)))
         (origin '(0 0 0))
         (best NIL))
    (do* ((step 10000000 (/ step 10))
          (start (list min-x min-y min-z) (list
                                            (- (first best) (* step 10))
                                            (- (second best) (* step 10))
                                            (- (third best) (* step 10))))
          (end (list max-x max-y max-z) (list
                                          (+ (first best) (* step 10))
                                          (+ (second best) (* step 10))
                                          (+ (third best) (* step 10)))))
        ((< step 1) (manhattan-distance-seq best origin))
      (let* ((points (3d-grid start end step))
             (wrapped (mapcar #'(lambda (p)
                                  (list
                                    (length (nanobots-in-range p nanobots))
                                    (manhattan-distance-seq p origin)
                                    p))
                              points))
             (sorted (sort
                       wrapped
                       #'(lambda (t1 t2)
                           (or (> (first t1) (first t2))
                               (and (= (first t1) (first t2))
                                    (< (second t1) (second t2))))))))
        (setf best (third (first sorted)))
        (format t "step: ~S~%" step)
        (format t "start: ~S~%" start)
        (format t "end ~S~%" end)
        (format t "best ~S~%" (first sorted))
        (format t "best-next ~S~%" (second sorted))
        (format t "~%")))))

(defun day23-2 ()
  (let* ((in (open "./day23.input"))
         (lst (read-by-line in))
         (result (solve-day23-2 (mapcar #'parse-nanobot lst))))
    (close in)
    result))

(defun parse-group-weaknesses-or-immunities (remaining)
  (let (ret)
    (loop :for item :in remaining
          :do (let* ((is-last (or (member #\) (coerce item 'list))
                                  (member #\; (coerce item 'list))))
                     (stripped (subseq item 0 (- (length item) 1))))
                (push stripped ret)
                (if is-last
                  (return ret))))))

(defun parse-group-weaknesses (parts)
  (let ((remaining (cddr (or (member "(weak" parts :test 'equal)
                             (member "weak" parts :test 'equal)))))
    (parse-group-weaknesses-or-immunities remaining)))

(defun parse-group-immunities (parts)
  (let ((remaining (cddr (or (member "(immune" parts :test 'equal)
                             (member "immune" parts :test 'equal)))))
    (parse-group-weaknesses-or-immunities remaining)))

(defun parse-group (str team)
  (let* ((parts (split-sequence:split-sequence #\Space str))
         (units (parse-integer (first parts)))
         (hits (parse-integer (fifth parts)))
         (remaining (nthcdr 7 parts))
         (weaknesses (parse-group-weaknesses remaining))
         (immunities (parse-group-immunities remaining))
         (remaining (rest (member "does" remaining :test 'equal)))
         (attack-damage (parse-integer (first remaining)))
         (attack-type (second remaining))
         (initiative (parse-integer (sixth remaining))))
    (list
      units
      hits
      attack-damage
      attack-type
      initiative
      weaknesses
      immunities
      team)))


(defun group-effective-power (group)
  (* (first group) (third group)))

(defun group-damage (attacking defending)
  (let ((attacking-ep (group-effective-power attacking))
        (attack-type (fourth attacking))
        (weaknesses (sixth defending))
        (immunities (seventh defending)))
    (cond ((member attack-type weaknesses :test 'equal) (* attacking-ep 2))
          ((member attack-type immunities :test 'equal) 0)
          (T attacking-ep))))

(defun find-group-to-attack (attacking all-groups)
  (let* ((team (eighth attacking))
         (other-groups (remove-if #'(lambda (attacking) (equal team (eighth attacking))) all-groups))
         (defendant (first
                      (sort other-groups
                            #'(lambda (a b)
                                (let ((damage-a (group-damage attacking a))
                                      (damage-b (group-damage attacking b))
                                      (ep-a (group-effective-power a))
                                      (ep-b (group-effective-power b))
                                      (initiative-a (fifth a))
                                      (initiative-b (fifth b)))
                                  (or (> damage-a damage-b)
                                      (and (= damage-a damage-b)
                                           (or (> ep-a ep-b)
                                               (and (= ep-a ep-b)
                                                    (> initiative-a initiative-b)))))))))))
    (if (not (zerop (group-damage attacking defendant)))
      defendant)))

(defun target-selection (all-groups)
  (let* ((selection (sort (copy-seq all-groups)
                          #'(lambda (a b)
                              (let ((a-ep (group-effective-power a))
                                    (b-ep (group-effective-power b))
                                    (a-initiative (fifth a))
                                    (b-initiative (fifth b)))
                                (or (> a-ep b-ep)
                                    (and (= a-ep b-ep)
                                         (> a-initiative b-initiative)))))))
         targets
         defendants)
    (loop :for attacking :in selection
          :do (let ((defendant (find-group-to-attack attacking (set-difference all-groups defendants))))
                (push (list attacking defendant) targets)
                (if defendant
                  (push defendant defendants))))
    targets))

(defun attack (targets)
  (let* ((all-groups (mapcar #'first targets))
         (sorted (sort (copy-seq targets) #'> :key #'(lambda (target)
                                                       (fifth (first target))))))
    (loop :for target :in sorted
          :when (second target)
          :do (let* ((attacking (first (member (first target) all-groups :test 'equalp)))
                     (defending (first (member (second target) all-groups :test 'equalp)))
                     (damage (group-damage attacking defending))
                     (killed (min (first defending) (floor damage (second defending)))))
                (when (> (first attacking) 0)
                  (setf (first defending) (- (first defending) killed)))))
    (remove-if #'(lambda (g) (zerop (first g))) all-groups)))

(defun immune-system-simulator (all-groups)
  (let* ((units-before (reduce #'+ (mapcar #'first all-groups)))
         (targets (target-selection all-groups))
         (remaining (attack targets))
         (units-after (reduce #'+ (mapcar #'first remaining))))
    (if (= units-before units-after)
      remaining
      (immune-system-simulator remaining))))

(defun solve-day24-1 (immune-groups infection-groups)
  (let ((remaining (immune-system-simulator (append immune-groups infection-groups))))
    (reduce #'+ (mapcar #'first remaining))))

(defun day24-1()
  (let* ((in1 (open "./day24immunities.input"))
         (in2 (open "./day24infection.input"))
         (lst1 (rest (read-by-line in1)))
         (lst2 (rest (read-by-line in2)))
         (result (solve-day24-1 (mapcar #'(lambda (s) (parse-group s 'immune-system)) lst1)
                                (mapcar #'(lambda (s) (parse-group s 'infection)) lst2))))
    (close in2)
    (close in1)
    result))

(defun apply-boost (boost group)
  (let ((copied (copy-seq group))
        (attack-damage (third group)))
    (setf (third copied) (+ attack-damage boost))
    copied))

(defun winning-boost-p (boost immune-groups infection-groups)
  (let* ((immune-groups-current (mapcar #'(lambda (g) (apply-boost boost g)) immune-groups))
         (infection-groups-current (mapcar #'copy-seq infection-groups))
         (remaining (immune-system-simulator (append immune-groups-current infection-groups-current))))
    (every #'(lambda (g) (equal (eighth g) 'immune-system)) remaining)))

(defun find-one-winning-boost (immune-groups infection-groups)
  (labels ((recurse (boost)
             (if (winning-boost-p boost immune-groups infection-groups)
               boost
               (recurse (* boost 2)))))
    (recurse 2)))

(defun find-min-winning-boost (winning-boost losing-boost immune-groups infection-groups)
  (let ((boost (floor (+ winning-boost losing-boost) 2)))
    (cond
      ((= boost losing-boost) winning-boost)
      ((winning-boost-p boost immune-groups infection-groups)
       (find-min-winning-boost boost losing-boost immune-groups infection-groups))
      (t (find-min-winning-boost winning-boost boost immune-groups infection-groups)))))

(defun solve-day24-2 (immune-groups infection-groups)
  (let* ((winning-boost (find-one-winning-boost immune-groups infection-groups))
         (losing-boost (/ winning-boost 2))
         (min-boost (find-min-winning-boost winning-boost losing-boost immune-groups infection-groups))
         (immune-groups-current (mapcar #'(lambda (g) (apply-boost min-boost g)) immune-groups)))
    (solve-day24-1 immune-groups-current infection-groups)))

(defun day24-2()
  (let* ((in1 (open "./day24immunities.input"))
         (in2 (open "./day24infection.input"))
         (lst1 (rest (read-by-line in1)))
         (lst2 (rest (read-by-line in2)))
         (result (solve-day24-2 (mapcar #'(lambda (s) (parse-group s 'immune-system)) lst1)
                                (mapcar #'(lambda (s) (parse-group s 'infection)) lst2))))
    (close in2)
    (close in1)
    result))


(defun make-disjointset (x)
  (let ((set (list x 0)))
    (setf (cdr (cdr set)) set)
    set))

(defun disjointset-find (x)
  (let ((parent (cdr (cdr x))))
    (if (eq parent x)
      x
      (setf (cdr (cdr x)) (disjointset-find parent)))))


(defun disjointset-union (x y)
  (let ((x-root (disjointset-find x))
        (y-root (disjointset-find y)))
    (cond ((> (second x-root) (second y-root))
           (setf (cdr (cdr y-root)) x-root))
          ((< (second x-root) (second y-root))
           (setf (cdr (cdr x-root)) y-root))
          ((not (eq x-root y-root))
           (setf (cdr (cdr y-root)) x-root)
           (incf (second x-root))))))

(defun solve-day25-1 (points)
  (let ((sets (mapcar #'make-disjointset points)))
    (loop :for s1 :in sets
          :do (loop :for s2 :in sets
                    :do (let* ((p1 (first s1))
                               (p2 (first s2))
                               (distance (manhattan-distance-seq p1 p2)))
                          (if (<= distance 3)
                            (disjointset-union s1 s2)))))
    (length
      (remove-duplicates
        (mapcar #'disjointset-find sets)
        :test 'eq))))

(defun day25-1 ()
  (let* ((in (open "./day25.input"))
         (lst (read-by-line in))
         (result (solve-day25-1 (mapcar
                                  (lambda (x) (parse-numbers #\, x))
                                  lst))))
    (close in)
    result))


(defun largest (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (elem (first x) (first x))
        (largest elem (if elem (max elem largest) largest)))
      ((null x) largest)))
