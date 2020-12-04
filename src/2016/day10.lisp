(defpackage :aoc/2016/10 #.cl-user::*aoc-use*)
(in-package :aoc/2016/10)

(defstruct bot id microchips logic)

(defun bot-microchips-sorted (bot)
  (destructuring-bind (a b) (bot-microchips bot)
    (if (< a b) (list a b) (list b a))))

(defun parse-init-instruction (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer value bot-id))
      ("value (\\d+) goes to bot (\\d+)" string)
    (list value bot-id)))

(defun parse-logic-instruction (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer bot-id)
                                  low-to-type
                                  (#'parse-integer low-to-id)
                                  high-to-type
                                  (#'parse-integer high-to-id))
      ("bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)" string)
    (list bot-id
          (string= low-to-type "bot") low-to-id
          (string= high-to-type "bot") high-to-id)))

(defun parse-bots (data)
  (let ((bots (make-hash-table)))
    (flet ((get-or-create (bot-id)
             (or (gethash bot-id bots)
                 (setf (gethash bot-id bots) (make-bot :id bot-id)))))
      (dolist (string data bots)
        (let (match)
          (cond ((setf match (parse-init-instruction string))
                 (destructuring-bind (value bot-id) match
                   (let ((bot (get-or-create bot-id)))
                     (push value (bot-microchips bot)))))
                ((setf match (parse-logic-instruction string))
                 (let ((bot (get-or-create (first match))))
                   (setf (bot-logic bot) (rest match))))))))))

(defun find-unblocked-bot (bots)
  (loop :for each :being :the :hash-values :of bots
        :for microchips = (bot-microchips each)
        :when (= (length microchips) 2) :return each))

(define-solution (2016 10) (bots parse-bots)
  (loop :with part1 :with outputs = (make-hash-table)
        :for bot = (find-unblocked-bot bots)
        :while bot
        :for (low high) = (bot-microchips-sorted bot)
        :when (and (= high 61) (= low 17)) :do (setf part1 (bot-id bot))
        :do (destructuring-bind (low-to-bot-p low-to-id high-to-bot-p high-to-id)
                (bot-logic bot)
              (if low-to-bot-p
                (push low (bot-microchips (gethash low-to-id bots)))
                (setf (gethash low-to-id outputs) low))
              (if high-to-bot-p
                (push high (bot-microchips (gethash high-to-id bots)))
                (setf (gethash high-to-id outputs) high))
              (setf (bot-microchips bot) nil))
        :finally (return (values part1
                                 (* (gethash 0 outputs)
                                    (gethash 1 outputs)
                                    (gethash 2 outputs))))))

(define-test (2016 10) (118 143153))
