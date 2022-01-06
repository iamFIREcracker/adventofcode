(defpackage :aoc/2021/23 #.cl-user::*aoc-use*)
(in-package :aoc/2021/23)


(defun parse-input (lines &aux
                          (rows (length lines))
                          (cols (length (car lines)))
                          (burrow (make-array (list rows cols)
                                              :element-type 'character
                                              :initial-element #\Space))
                          rooms)
  (loop for row from 0 for string in lines do
        (loop for col from 0 for ch across string do
              (setf (aref burrow row col) ch)
              (when (and (< 1 row (1- rows)) (member col '(3 5 7 9)))
                (let* ((type (case col (3 #\A) (5 #\B) (7 #\C) (9 #\D)))
                       (existing (assoc type rooms)))
                  (if existing
                    (push (list row col) (second existing))
                    (push (list type (list (list row col))) rooms))))))
  (cons burrow rooms))
(defun siderooms (rooms type) (second (assoc type rooms)))


(defparameter *hallway* '((1 1) (1 2) (1 4) (1 6) (1 8) (1 10) (1 11)))


(defun organize (input &aux (burrow (car input)) (rooms (cdr input)))
  (search-cost
    (dijkstra burrow :test 'equalp :goalp (partial-1 #'donep rooms)
              :neighbors (partial-1 #'possible-moves rooms))))


(defun donep (rooms burrow)
  (loop for (type siderooms) in rooms always
        (loop for (row col) in siderooms
              always (char= (aref burrow row col) type))))


(defun possible-moves (rooms burrow &aux moves)
  (labels ((add-move (type pos target &aux
                           (next (copy-array burrow))
                           (cost (* (manhattan-distance pos target)
                                    (ecase type (#\A 1) (#\B 10) (#\C 100) (#\D 1000)))))
             (rotatef (aref next (car pos) (cadr pos))
                      (aref next (car target) (cadr target)))
             (push (cons next cost) moves)))
    ;; From the wrong sideroom to the hallway
    (loop for (_ positions) in rooms do
          (loop for pos in positions for type = (aref burrow (car pos) (cadr pos))
            unless (or (char= type #\.) (in-place-p rooms burrow pos)) do
            (loop for target in *hallway*
                  unless (blockedp burrow pos target) do (add-move type pos target))))
    ;; Fromt he hallway to the right sideroom
    (loop for pos in *hallway* for type = (aref burrow (car pos) (cadr pos))
          unless (char= type #\.) do
          (loop for target in (siderooms rooms type)
            unless (blockedp burrow pos target) do (add-move type pos target)
            always (eql (aref burrow (car target) (cadr target)) type)))
    moves))


(defun in-place-p (rooms burrow pos
                         &aux (type (aref burrow (car pos) (cadr pos))))
  (loop for r in (siderooms rooms type)
        thereis (equal r pos)
        ;; if not in the room, at least let's make sure the one below is
        ;; occupied by an amphipod of the right type
        always (eql (aref burrow (car r) (cadr r)) type)))


(defun blockedp (burrow pos target)
  (destructuring-bind (row1 col1) pos
    (destructuring-bind (row2 col2) target
      (let ((col-step (<=> col2 col1)))
        (flet ((check-all (row1 col1 row2 col2 row-step col-step)
                 (loop do (incf row1 row-step) (incf col1 col-step)
                       thereis (char/= (aref burrow row1 col1) #\.)
                       until (and (= row1 row2) (= col1 col2)))))
          (if (= row1 1)
            (or (check-all row1 col1 row1 col2 0 col-step)
                (check-all row1 col2 row2 col2 1 0))
            (or (check-all row1 col1 row2 col1 -1 0)
                (check-all row2 col1 row2 col2 0 col-step))))))))


(defun massage (lines)
  (append
    (subseq lines 0 3)
    (list
      "  #D#C#B#A#"
      "  #D#B#A#C#")
    (subseq lines 3)))


(define-solution (2021 23) (lines)
  (values (organize (parse-input lines)) (organize (parse-input (massage lines)))))

(define-test (2021 23) (18282 50132))
