(defun read-by-line (in)
  (loop for i = (read-line in nil :eof)
      until (eq i :eof)
      collect i))


(defun list-to-string (lst)
  "Convert a list into a string of concatenated characters"
  (format nil "~{~A ~}" lst))

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


(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))


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

(defun manhattan-distance-seq (seq1 seq2)
  (reduce #'+ (mapcar #'abs (mapcar #'- seq1 seq2))))

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


(defun largest (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (elem (first x) (first x))
        (largest elem (if elem (max elem largest) largest)))
      ((null x) largest)))
