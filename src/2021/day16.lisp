(defpackage :aoc/2021/16 #.cl-user::*aoc-use*)
(in-package :aoc/2021/16)


(defun parse-input (data)
  (parse-packet (hexadecimal->binary (first data))))


(defun bin (s start &optional (end (length s)))
  (parse-integer s :start start :end end :radix 2))


(defun parse-packet (s &optional (start 0))
  (let ((version (bin s start (incf start 3)))
        (type (bin s start (incf start 3))))
    (multiple-value-bind (parsed start)
        (if (= type 4) (parse-literal s start) (parse-operator s start))
      (values (list :version version
                    :type type
                    :literal (if (= type 4) parsed)
                    :sub-packets (if (/= type 4) parsed))
              start))))


(defun parse-literal (s &optional (start 0) &aux (literal 0))
  (loop
    (let ((morep (= (bin s start (incf start)) 1)))
      (setf literal (+ (* literal 16) (bin s start (incf start 4))))
      (unless morep (return (values literal start))))))


(defun parse-operator (s &optional (start 0) &aux pkts)
  (ecase (bin s start (incf start))
    (0 (let* ((remaining (bin s start (incf start 15))))
         (loop while (> remaining 3) do
               (multiple-value-bind (pkt consumed) (parse-packet s start)
                 (setf remaining (- remaining (- consumed start))
                       pkts (cons pkt pkts)
                       start consumed)))))
    (1 (dotimes (_ (bin s start (incf start 11)))
         (multiple-value-bind (pkt consumed) (parse-packet s start)
           (setf pkts (cons pkt pkts) start consumed)))))
  (values (reverse pkts) start))


(defun part1 (pkt)
  (labels ((recur (pkt)
             (+ (getf pkt :version)
                (loop for next in (getf pkt :sub-packets)
                      sum (recur next)))))
    (recur pkt)))


(defun b> (a b) (if (> a b) 1 0))
(defun b< (a b) (if (< a b) 1 0))
(defun b= (a b) (if (= a b) 1 0))

(defun part2 (pkt)
  (labels ((recur (pkt &aux (literal (getf pkt :literal)))
             (cond (literal literal)
                   (t (apply ; replace with CONS to _visualize_ your solution
                        (aref #(+ * min max identity b> b< b=) (getf pkt :type))
                        (loop for next in (getf pkt :sub-packets)
                              collect (recur next)))))))
    (recur pkt)))


(define-solution (2021 16) (pkt parse-input)
  (values (part1 pkt) (part2 pkt)))

(define-test (2021 16) (943 167737115857))
