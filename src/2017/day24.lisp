(defpackage :aoc/2017/24 #.cl-user::*aoc-use*)
(in-package :aoc/2017/24)

(defun parse-ports (x &aux (ports (make-hash-table)))
  (flet ((parse-port (s &aux (splits (split-sequence:split-sequence #\/ s)))
           (mapcar #'parse-integer splits)))
    (loop
      :for (a b) :in (mapcar #'parse-port x)
      :do (push b (gethash a ports))
      :unless (= a b) :do (push a (gethash b ports)))
    ports))

(defun gen-bridges (ports)
  (labels ((id (a b)
             (if (> a b) (complex b a) (complex a b)))
           (possible-outputs (a visited)
             (loop
               :for b :in (gethash a ports)
               :for id = (id a b)
               :unless (member id visited)
               :collect b))
           (recur (a visited)
             (let ((outputs (possible-outputs a visited)))
               (if (null outputs)
                 (list NIL)
                 (loop
                   :for b :in outputs
                   :for id = (id a b)
                   :for rest = (recur b (cons id visited))
                   :append (mapcar (partial-1 #'cons id) rest))))))
    (recur 0 nil)))

(defun solve-part1 (bridges)
  (labels ((strength (bridge)
             (loop
               :for p :in bridge
               :sum (+ (realpart p) (imagpart p)))))
    (loop
      :for bridge :in bridges
      :maximize (strength bridge))))

(define-solution (2017 24) (data parse-ports)
  (let ((bridges (gen-bridges data)))
    (values
      (solve-part1 bridges)
      (flet ((longest (bridges)
               (loop
                 :for bridge :in bridges
                 :maximize (length bridge) :into longest
                 :finally (return (remove-if-not (partial-1 #'= longest) bridges :key #'length)))))
        (solve-part1 (longest bridges))))))

(define-test (2017 24) (2006 1994))
