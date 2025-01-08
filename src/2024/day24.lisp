(defpackage :aoc/2024/24 #.cl-user::*aoc-use*)
(in-package :aoc/2024/24)

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day24.txt")))
  (destructuring-bind (initial-wires connections)
                      (split-sequence:split-sequence "" strings :test 'equal)
    (list
      ;; inputs
      (prog1-let map (make-hash-table :test 'equal)
        (dolist (s initial-wires)
          (setf (gethash (subseq s 0 3) map)
                (parse-integer (subseq s 5)))))
      ;; output->inputs
      (prog1-let map (make-hash-table :test 'equal)
        (dolist (s connections)
          (destructuring-bind (in1 gate in2 _ output)
                              (split-sequence:split-sequence #\Space s)
            (declare (ignore _))
            (setf (gethash output map) (list in1 gate in2))))))))
#+#:excluded (parse-input)


(defun run (&optional (input (parse-input)))
  (destructuring-bind (values outputs) input
    (flet ((value (name)
             (recursively ((curr name))
               (cond ((gethash curr values) (gethash curr values))
                     (t (destructuring-bind (in1 gate in2) (gethash curr outputs)
                          (setf (gethash curr values)
                                (epcase string= gate
                                  ("AND" (logand (recur in1) (recur in2)))
                                  ("OR" (logior (recur in1) (recur in2)))
                                  ("XOR" (logxor (recur in1) (recur in2)))))))))))
      (let1 outputs (~>> (hash-table-keys outputs)
                         (keep-if [string-starts-with-p "z" %])
                         (sort ~ #'string>))
        (parse-integer
          (looping
            (doseq (out outputs)
              (spr! ~out.value)))
          :radix 2)))))

;; We know our input is meant to represent a 45-bit full-adder, and that there
;; are 8 gates that were swapped.
;;
;; Here is one way to tackle the problem:
;;
;; 1. For each output gate Z, figure out what the _correct_ network having
;;    X and Y as inputs should look like -- it's a full-adder, so we can easily
;;    grab the spec online, and derive a function for it (EXPECTED-NTH)
;; 2. Compare each output's expected network with the _actual_ one (from the input),
;;    to locate which gates are correctly placed and which ones are not
;; 3. For each misplaced gate, collect the all the gates contributing to them,
;;    as the swap might happen not just at the output layer, but even in the
;;    inner ones
;; 4. Try to swap all pairs of misplaced gates until the _actual_ network
;;    looks like the _expected_ version

(defun wire (name n)
  (if (< n 10) (spr name 0 n) (spr name n)))
#+#:excluded (string= (wire "z" 0) "z00")
#+#:excluded (string= (wire "x" 19) "x19")

(defun expected-nth (n)
  (labels ((carry-th (n)
             (cond
               ((= n 0) `(logand ,(wire "y" n) ,(wire "x" n)))
               (t `(logior
                     (logand ,(sum-th n)
                             ,(carry-th (1- n)))
                     (logand ,(wire "y" n) ,(wire "x" n))))))
           (sum-th (n &optional)
             `(logxor ,(wire "y" n) ,(wire "x" n))))
    (if (plusp n)
        `(logxor ,(carry-th (1- n)) ,(sum-th n))
        (sum-th n))))
#+#:excluded (equal (expected-nth 0)
                    `(logxor "y00" "x00"))
#+#:excluded (equal (expected-nth 1)
                    `(logxor (logand "y00" "x00") (logxor "y01" "x01")))
#+#:excluded (equal (expected-nth 2)
                    `(logxor
                       (logior (logand (logxor "y01" "x01") (logand "y00" "x00"))
                               (logand "y01" "x01"))
                       (logxor "y02" "x02")))

(defun actual-nth (n &optional (outputs (second (parse-input))))
  (recursively ((curr (wire "z" n))
                (depth 0))
    (cond
      ((string-starts-with-p "x" curr) curr)
      ((string-starts-with-p "y" curr) curr)
      ((= depth 100) :loop-detected)
      (t (destructuring-bind (in1 gate in2) (gethash curr outputs)
           (epcase string= gate
             ("AND" `(logand ,(recur in1 ~depth.1+) ,(recur in2 ~depth.1+)))
             ("OR" `(logior ,(recur in1 ~depth.1+) ,(recur in2 ~depth.1+)))
             ("XOR" `(logxor ,(recur in1 ~depth.1+) ,(recur in2 ~depth.1+)))))))))
#+#:excluded (equal (actual-nth 0) `(logxor "y00" "x00"))
#+#:excluded (equal (actual-nth 1) `(logxor (logand "y00" "x00") (logxor "y01" "x01")))
#+#:excluded (equal (actual-nth 2)
                    `(logxor
                       (logior (logand (logxor "y01" "x01") (logand "y00" "x00"))
                               (logand "y01" "x01"))
                       (logxor "x02" "y02")))


(defun same-expansion? (sx1 sx2)
  (labels ((canonicalize-expansion (x)
             (recursively ((x x))
               (cond ((listp x) (sort (mapcar #'recur x) #'< :key #'sxhash))
                     (t x)))))
    (equal (canonicalize-expansion sx1) (canonicalize-expansion sx2))))

(defun alright-nth? (n &optional (outputs (second (parse-input))))
  (same-expansion? (expected-nth n) (actual-nth n outputs)))
#+#:excluded (looping
               (dotimes (n 44)
                 (let1 alright? (alright-nth? n)
                   (dbgl n alright?)
                   (thereis! (not alright?)))))


(defun gates-for-nth (n &optional (outputs (second (parse-input))))
  (looping
    (recursively ((curr (wire "z" n)))
      (cond
        ((string-starts-with-p "x" curr) nil)
        ((string-starts-with-p "y" curr) nil)
        (t (adjoin! curr)
           (destructuring-bind (in1 gate in2) (gethash curr outputs)
             (declare (ignore gate))
             (recur in1)
             (recur in2)))))))

(defun find-swaps (&optional (input (parse-input))
                   &aux (outputs ~input.second))
  (let1 gates (hash-table-keys outputs)
    (recursively ((n 0)
                  (gates gates)
                  (swapped nil))
      (cond ((and (= n 44) (= (length swapped) 8)) (return swapped))
            ((alright-nth? n outputs)
              (recur (1+ n)
                     (set-difference gates (gates-for-nth n outputs) :test 'equal)
                     swapped))
            (t (dosublists ((gate1 . rem1) gates)
                 (dolist (gate2 rem1)
                   (rotatef (gethash gate1 outputs)
                            (gethash gate2 outputs))
                   (when (alright-nth? n outputs)
                     (assert (looping
                               (dotimes (m n)
                                 (always! (alright-nth? n outputs)))))
                     (recur (1+ n)
                            (set-difference gates (gates-for-nth n outputs) :test 'equal)
                            (list* gate1 gate2 swapped)))
                   (rotatef (gethash gate1 outputs)
                            (gethash gate2 outputs)))))))))

(define-solution (2024 24) (input parse-input)
  (values (run input)
          (~> input find-swaps (sort ~ #'string<) (format nil "~{~A~^,~}" ~))))

(define-test (2024 24) (51410244478064 "gst,khg,nhn,tvb,vdc,z12,z21,z33"))
