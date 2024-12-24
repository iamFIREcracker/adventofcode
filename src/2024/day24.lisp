(defpackage :aoc/2024/24 #.cl-user::*aoc-use*)
(in-package :aoc/2024/24)

(declaim (optimize (debug 3)))

(defun parse-input (&optional (strings (uiop:read-file-lines #P"src/2024/day24.txt")))
  (destructuring-bind (connections initial-wires)
      (split-sequence:split-sequence "" strings :test 'equal)
    (list
      ;; values
      (prog1-let (map (make-hash-table :test 'equal))
        (dolist (s connections)
          (setf (gethash (subseq s 0 3) map)
                (parse-integer (subseq s 5)))))
      ;; inputs
      (prog1-let (map (make-hash-table :test 'equal))
        (dolist (s initial-wires)
          (destructuring-bind (in1 gate in2 _ output)
              (split-sequence:split-sequence #\Space s)
            (declare (ignore _))
            (setf (gethash in1 map) (list in1 gate in2 output)
                  (gethash in2 map) (list in1 gate in2 output) ))))
      ;; outputs
      (prog1-let (map (make-hash-table :test 'equal))
        (dolist (s initial-wires)
          (destructuring-bind (in1 gate in2 _ output)
              (split-sequence:split-sequence #\Space s)
            (declare (ignore _))
            (setf (gethash output map) (list in1 gate in2 output))))))))
#+#:excluded (parse-input)

(defun run (&optional (input (parse-input)))
  (destructuring-bind (values inputs outputs) input
    (declare (ignore inputs))
    (flet ((value (name)
             (recursively ((curr name))
               (cond ((gethash curr values) (gethash curr values))
                     (t (destructuring-bind (in1 gate in2 out) (gethash curr outputs)
                          (declare (ignore out))
                          (setf (gethash curr values)
                                (cond ((string= gate "AND") (logand (recur in1) (recur in2)))
                                      ((string= gate "OR") (logior (recur in1) (recur in2)))
                                      ((string= gate "XOR") (logxor (recur in1) (recur in2)))))))))))
      (~> (hash-table-keys outputs)
          (keep-if [string-starts-with-p "z" _] ~)
          (sort ~ #'string>)
          (mapcar [value _] ~)
          (apply #'spr ~)
          (parse-integer ~ :radix 2)
          ))))
(run)
;51410244478064!


(defun wires (name &optional (input (parse-input)))
  (or (~> (hash-table-keys (third input))
          (keep-if [string-starts-with-p name _] ~)
          (sort ~ #'string>))
      (~> (hash-table-keys (second input))
          (keep-if [string-starts-with-p name _] ~)
          (sort ~ #'string>))))
(wires "z")
(wires "y")
(wires "x")

(defun in->out (out &optional (outputs (third (parse-input))))
  (recursively ((curr out)
                (rec-limit 100))
    (decf rec-limit)
    (when (plusp rec-limit)
      (cond
        ((string-starts-with-p "x" curr) curr)
        ((string-starts-with-p "y" curr) curr)
        (t (destructuring-bind (in1 gate in2 out) (gethash curr outputs)
             (declare (ignore out))
             (cond ((string= gate "AND") `(logand ,(recur in1 rec-limit) ,(recur in2 rec-limit)))
                   ((string= gate "OR") `(logior ,(recur in1 rec-limit) ,(recur in2 rec-limit)))
                   ((string= gate "XOR") `(logxor ,(recur in1 rec-limit) ,(recur in2 rec-limit))))))))))
(in->out "z00")
; (LOGXOR "y00" "x00")

(in->out "z01")
;(LOGXOR (LOGAND "y00" "x00") (LOGXOR "y01" "x01"))

(in->out "z02")
; (LOGXOR
;  ;; carry out previous step
;  (LOGIOR (LOGAND (LOGXOR "y01" "x01") (LOGAND "y00" "x00"))
;          (LOGAND "y01" "x01"))
;  ;; new bits
;  (LOGXOR "x02" "y02"))

; sum i
#+#:excluded (logxor x-i y-i)
; carri i
#+#:excluded (LOGIOR
               (LOGAND (sum i-1)
                       (carry i-1))
               (LOGAND "y01" "x01"))

(defun input-th (name n)
  (if (< n 10) (spr name 0 n) (spr name n)))
#+#:excluded (input-th "z" 0)

(defun sum-th (n &optional)
  `(logxor ,(input-th "y" n) ,(input-th "x" n)))
#+#:excluded (sum-th 0)
#+#:excluded (sum-th 3)

(defun carry-th (n)
  (cond
    ((= n 0) `(logand ,(input-th "y" n) ,(input-th "x" n)))
    ;; XXX does the order matter?
    (t `(logior
          (logand ,(sum-th n)
                  ,(carry-th (1- n)))
          (logand ,(input-th "y" n) ,(input-th "x" n))))))
#+#:excluded (pprint (carry-th 2))

(defun out-th (n)
  (if (plusp n)
      `(logxor ,(carry-th (1- n)) ,(sum-th n))
      (sum-th n)))
#+#:excluded (out-th 0)
#+#:excluded (out-th 1)

(defun same-expansion? (sx1 sx2)
  (cond
    ((xor (atom sx1) (atom sx2)) nil)
    ((xor (stringp sx1) (stringp sx2)) nil)
    ((xor (listp sx1) (listp sx2)) nil)
    ((atom sx1) (equal sx1 sx2))
    ((stringp sx1) (equal sx1 sx2))
    (t (destructuring-bind (rator1 rand11 rand12) sx1
         (destructuring-bind (rator2 rand21 rand22) sx2
           (and (same-expansion? rator1 rator2)
                (or (and (same-expansion? rand11 rand21)
                         (same-expansion? rand12 rand22))
                    (and (same-expansion? rand11 rand22)
                         (same-expansion? rand12 rand21)))))))))

(defun swap (outputs gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8)
  (prog1-let (copy (copy-hash-table outputs))
    (rotatef (gethash gate1 copy)
             (gethash gate2 copy))
    (rotatef (gethash gate3 copy)
             (gethash gate4 copy))
    (rotatef (gethash gate5 copy)
             (gethash gate6 copy))
    (rotatef (gethash gate7 copy)
             (gethash gate8 copy))))

#;
(block solve
  (let1 outputs (third (parse-input))
    (let1 out-keys (hash-table-keys outputs)
      (dosublists ((gate1 . rem1) out-keys)
        (dbgl gate1 (length rem1))
        (dosublists ((gate2 . rem2) rem1)
          (dosublists ((gate3 . rem3) rem2)
            (dosublists ((gate4 . rem4) rem3)
              (dosublists ((gate5 . rem5) rem4)
                (dosublists ((gate6 . rem6) rem5)
                  (dosublists ((gate7 . rem7) rem6)
                    (dosublists ((gate8 . rem8) rem7)
                      (when (= (length (remove-duplicates (list gate1 gate2 gate3 gate4
                                                                gate5 gate6 gate7 gate8)
                                                          :test 'equal)) 8)
                        (let1 swapped (swap outputs
                                            gate1 gate2 gate3 gate4
                                            gate5 gate6 gate7 gate8)
                          (when (looping
                                  (dotimes (n 44)
                                    (always! (same-expansion? (out-th n)
                                                              (in->out (input-th "z" n) swapped)))))
                            (return-from solve (format t "~{~A~^,~}" (sort (list gate1 gate2 gate3 gate4
                                                                                   gate5 gate6 gate7 gate8)
                                                                             #'string<)))
                            ))))))))))))))
#+#:excluded (let1 outputs (third (parse-input))
               (dotimes (n 44)
                 (dbgl n)
                 (continuable
                   (dbgl (gates-for (input-th "z" n)) n outputs)
                   (assert (same-expansion? (out-th n)
                                            (in->out (input-th "z" n) outputs))))))
(defun alright? (n outputs)
  (same-expansion? (out-th n)
                   (in->out (input-th "z" n) outputs)))

(defun gates-for (out &optional (outputs (third (parse-input))))
  (looping
    (recursively ((curr out))
      (cond
        ((string-starts-with-p "x" curr) nil)
        ((string-starts-with-p "y" curr) nil)
        (t (destructuring-bind (in1 gate in2 out) (gethash curr outputs)
            (declare (ignore gate))
            (adjoin! out)
            (recur in1)
            (recur in2)))))))
#+#:excluded (gates-for "z00")
#+#:excluded (gates-for "z01")
#+#:excluded (gates-for "z04")
#+#:excluded (gates-for "z45")



(defun canonicalize (gates) (sort (copy-seq gates) #'string<))

(defun solve (&optional (outputs (third (parse-input))))
  (let1 gates (hash-table-keys outputs)
    (recursively ((n 0)
                  (gates gates)
                  (swapped nil))
      (cond ((and (= n 44) (= (length swapped) 8)) (format t "~{~A~^,~}" (canonicalize swapped)))
            ((alright? n outputs)
              (recur (1+ n)
                     (set-difference gates (gates-for (input-th "z" n) outputs) :test 'equal)
                     swapped))
            (t (dosublists ((gate1 . rem1) gates)
                 (dolist (gate2 rem1)
                   (rotatef (gethash gate1 outputs)
                            (gethash gate2 outputs))
                   (when (alright? n outputs)
                     (dbgl n gate1 gate2 swapped)
                     (assert (looping
                               (dotimes (m n)
                                 (always! (alright? n outputs)))))
                     (recur (1+ n)
                            (set-difference gates (gates-for (input-th "z" n) outputs) :test 'equal)
                            (list* gate1 gate2 swapped)))
                   (rotatef (gethash gate1 outputs)
                            (gethash gate2 outputs)) ))))
      )))
(solve)
gst,khg,nhn,tvb,vdc,z12,z21,z33
