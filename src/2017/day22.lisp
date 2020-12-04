(defpackage :aoc/2017/22 #.cl-user::*aoc-use*)
(in-package :aoc/2017/22)


(defun parse-infection-map (x &aux (map (make-hash-table)))
  (let* ((offset-y (truncate (length x) 2))
         (offset-x (truncate (length (first x)) 2)))
    (dorange (i 0 (length x))
      (dorange (j 0 (length (first x)))
        (hash-table-insert map (complex (- j offset-x) (- offset-y i)) (aref (nth i x) j))))
    map))

(defun cleanp (node)
  (or (not node) (eql node #\.)))

(defun weakenedp (node)
  (eql node #\W))

(defun infectedp (node)
  (eql node #\#))

(defun flaggedp (node)
  (eql node #\F))

(defun run-infection (data bursts update-node)
  (flet ((update-dir (node dir)
           (cond ((cleanp node) (complex-rotate-ccw dir))
                 ((weakenedp node) dir)
                 ((infectedp node) (complex-rotate-cw dir))
                 ((flaggedp node) (* dir -1)))))
    (loop
      :with grid = (parse-infection-map data)
      :with curr = #C(0 0)
      :with dir = #C(0 1)
      :repeat bursts
      :for node = (gethash curr grid)
      :for dir-next = (update-dir node dir)
      :for node-next = (funcall update-node node)
      :count (infectedp node-next)
      :do (setf dir dir-next
                (gethash curr grid) node-next
                curr (+ curr dir)))))

(define-solution (2017 22) (data)
  (values
    (run-infection data 10000
                   #'(lambda (node)
                       (if (infectedp node) #\. #\#)))
    (run-infection data 10000000
                   #'(lambda (node)
                       (cond ((cleanp node) #\W)
                             ((weakenedp node) #\#)
                             ((infectedp node) #\F)
                             ((flaggedp node) #\.))))))

(define-test (2017 22) (5570 2512022))
