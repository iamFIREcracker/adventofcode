(defpackage :aoc/2024/06 #.cl-user::*aoc-use*)
(in-package :aoc/2024/06)

#;

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day06.txt")))
  (let ((map (make-hash-table :test 'equal))
        start)
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (let1 coord (list i j)
          (ecase ch
            ((#\# #\.) (setf (@ map coord) ch))
            (#\^ (setf (@ map coord) #\^
                       start coord))))))
    (list map start (list -1 0))))
#+#:excluded (parse-input)

(defun move-straight (dir pos) (mapcar #'+ pos dir))
(defun rotate-cw (dir) (list (second dir) (- (first dir))))
(defun rotate-ccw (dir) (list (- (second dir)) (first dir)))

;; Part1
#+#:excluded (destructuring-bind (map start dir) (parse-input)
               (let ((pos start)
                     (seen (make-hash-table :test 'equal)))
                 (while (@ map pos)
                   (setf (@ seen pos) t)
                   (while (equal (@ map (move-straight dir pos)) #\#)
                     (setf dir (rotate-cw dir)))
                   (setf pos (move-straight dir pos)))
                 (hash-table-count seen)))
; 5331

;;; Part 2
(defun path (map pos dir &optional (loop-error-p t) (loop-error-value nil))
  (let1 path (make-hash-table :test 'equal)
    (while (@ map pos)
      (when (@ path (list pos dir))
        (if loop-error-p
          (error "Loop detected!")
          (return-from path loop-error-value)))
      (setf (@ path (list pos dir)) t)
      (while (equal (@ map (move-straight dir pos)) #\#)
        (setf dir (rotate-cw dir)))
      (setf pos (move-straight dir pos)))
    path))
#+#:excluded (apply 'path (parse-input))
#+#:excluded (length (remove-duplicates (hash-table-keys (apply 'path (parse-input))) :key 'car :test 'equal))

(destructuring-bind (map pos dir) (parse-input)
  (let1 obstructions (make-hash-table :test 'equal)
    (dohash ((pos1 dir1) _ (path map pos dir))
      (let1 pos2 (move-straight pos1 dir1)
        (when-let (ch (@ map pos2))
          (when (and (char= ch #\.) (not (@ obstructions pos2)))
            (setf (@ map pos2) #\#)
            (if-not (path map pos dir nil nil)
              (setf (@ obstructions pos2) t))
            (setf (@ map pos2) #\.)))))
    (hash-table-count obstructions)))
; 1721 too low
; 1722 too low
; 1771 too low
; 1770 wtf?! not sure why i tried this...


;; the above does not work with this minimal example:
;;
;;  012
;; 0.#.
;; 1...
;; 2#^#
;; 3###
;;
;; in this, the algo would not try to place an obstruction
;; on (1 2) which would indeed create a loop!
;;
;; that's because we are only checking if the _next_ position
;; is empty:
;;
;; pos   | dir    | next   | obstraction candidate |
;; -------------------------------------------------
;; (2 1) | (-1 0) | (1 2)  | yes (empty slot)      |
;; (1 1) | (-1 0) | (0 2)  | no  (already obstr)   |
;; (1 2) | (0 1)  | (1 3)  | no (out of grid)      |
;;
;; We failed to test for changes of directions
;;
;; better bruteforce all the path positions, except for the 
;; starting point
(destructuring-bind (map pos dir) (parse-input)
  (let1 obstructions (make-hash-table :test 'equal)
    (dohash ((pos1 dir1) _ (path map pos dir))
      (unless (equal pos1 pos)
        (when-let (ch (@ map pos1))
          (when (and (char= ch #\.) (not (@ obstructions pos1)))
            (setf (@ map pos1) #\#)
            (if-not (path map pos dir nil nil)
              (setf (@ obstructions pos1) t))
            (setf (@ map pos1) #\.)))))
    (hash-table-count obstructions)))
#+#:excluded 1812
