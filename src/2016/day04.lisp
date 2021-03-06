(defpackage :aoc/2016/04 #.cl-user::*aoc-use*)
(in-package :aoc/2016/04)

(defun read-single-room-specification (string)
  (cl-ppcre:register-groups-bind (name (#'parse-integer sector) checksum)
      ("([a-z\\-]+)-(\\d+)\\[([a-z-]+)\]" string)
    (list name sector checksum)))

(defun read-specifications (data)
  (mapcar #'read-single-room-specification data))

(defun validp (freqs checksum)
  (setf freqs (sort freqs (lambda (e1 e2)
                            (destructuring-bind ((c1 . f1) (c2 . f2))
                                (list e1 e2)
                              (or (> f1 f2)
                                  (and (= f1 f2)
                                       (char< c1 c2)))))))
  (and (<= (length checksum) (length freqs))
       (loop :for e :across checksum
             :for (c) :in freqs
             :always (eql e c))))

(defun decipher-char (c offset &aux (base (char-code #\a)))
  (->< (char-code c)
    (- >< base)
    (+ offset)
    (mod >< 26)
    (+ base)
    code-char))

(defun caesar-decipher (string offset)
  (map 'string #'(lambda (c)
                   (if (eql c #\-) #\Space (decipher-char c offset)))
       string))

(define-solution (2016 4) (specs read-specifications)
  (loop :with part2
        :for (name sector checksum) :in specs
        :for freqs = (frequencies (remove #\- name))
        :for deciphered = (caesar-decipher name sector)
        :when (validp freqs checksum) :sum sector :into part1
        :when (and (not part2) (search "northpole" deciphered)) :do (setf part2 sector)
        :finally (return (values part1 part2))))

(define-test (2016 4) (137896 501))
