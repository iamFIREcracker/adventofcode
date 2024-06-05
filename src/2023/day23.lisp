(defpackage :aoc/2023/23 #.cl-user::*aoc-use*)
(in-package :aoc/2023/23)


(defparameter *north* '(-1 0))
(defparameter *east*  '(0 1))
(defparameter *south* '(1 0))
(defparameter *west*  '(0 -1))

(defun move-straight (pos dir) (mapcar #'+ pos dir))


(defun parse-map (&optional (strings (aoc::read-problem-input 2023 23)))
  (bnd* ((rows (length strings))
         (cols (length (first strings)))
         (map (make-hash-table :test 'equal)))
    (doseq ((i s) (enumerate strings))
      (doseq ((j ch) (enumerate s))
        (when (find ch ".^>v<")
          (setf (gethash (list i j) map) ch))))
    (list map (list 0 1) (list (1- rows) (- cols 2)))))


(defun valid-directions (map pos)
  (looping
    (dolist (dir (list *north* *east* *south* *west*))
      (bnd1 npos (move-straight pos dir)
        (when (gethash npos map)
          (collect! dir))))))

(defun intersection? (map pos)
  (> (length (valid-directions map pos)) 2))

(defun intersections (&optional (map (car (parse-map))))
  (looping
    (dolist (pos (hash-table-keys map))
      (when (intersection? map pos)
        (collect! pos)))))


(defun compress-distances (&optional (input (parse-map)))
  (destructuring-bind (map start end) input
    (bnd* ((all-points (list* start end (intersections map)))
           (cache ()))
      (flet ((cache-set (from to d)
               (setf (assoc-value (assoc-value cache from) to) d)))
        (dolist (start all-points)
          (bnd* ((q (make-queue))
                 (seen (make-hash-table :test 'equal)))
            (enqueue (cons start 0) q)
            (while-not (queue-empty-p q)
              (bnd1 (pos . d) (dequeue q)
                (unless-already-seen (seen pos)
                  (if (and (not (equal pos start))
                           (member pos all-points :test #'equal))
                    (cache-set start pos d)
                    (doseqs ((dir (list *north* *east* *south* *west*))
                             (ch "^>v<"))
                      (bnd1 npos (move-straight pos dir)
                        (awhen (gethash npos map)
                          (when (or (char= it #\.)
                                    ;; can move to a ^ only by going north
                                    ;; can move to a > only by going west
                                    ;; ...
                                    (char= it ch))
                            (enqueue (cons npos (1+ d)) q))))))))))))
      cache)))


(defun find-longest-path (&optional (strings (aoc::read-problem-input 2023 23)))
  (bnd* ((input (parse-map strings))
         (cache (compress-distances input))
         (seen (make-hash-table :test 'equal)))
    (destructuring-bind (start end) (cdr input)
      (looping
        (recursively ((pos start)
                      (d 0))
          (unless-already-seen (seen pos)
            (if (equal pos end)
              (maximize! d)
              (doseq ((npos . nd) (assoc-value cache pos))
                (recur npos (+ d nd))))
            (unsee)))))))


(defun massage-input (&optional (strings (aoc::read-problem-input 2023 23)))
  (flet ((replace-slope (s) (substitute-if #\. [find _ ">v<^"] s)))
    (mapcar #'replace-slope strings)))


(define-solution (2023 23) (strings)
  (values (find-longest-path strings)
          (find-longest-path (massage-input strings))))

(define-test (2023 23) (2018 6406))
