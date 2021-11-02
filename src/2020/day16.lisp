(defpackage :aoc/2020/16 #.cl-user::*aoc-use*)
(in-package :aoc/2020/16)

(defun parse-ticket-rule (string)
  (cl-ppcre:register-groups-bind (name (#'parse-integer from1 to1 from2 to2))
      ("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" string)
    (list name (cons from1 to1) (cons from2 to2))))

(defun parse-ticket-rules (data) (mapcar #'parse-ticket-rule data))

(defun parse-ticket (string)
  (map 'vector #'parse-integer (split-sequence:split-sequence #\, string)))

(defun parse-your-ticket (data) (parse-ticket (second data))) ; skip header

(defun parse-nearby-tickets (data)
  (mapcar #'parse-ticket (rest data))) ; skip header

(defun parse-document (data)
  (let (groups current)
    (dolist (string (append data '("")))
      (if (string= string "")
        (setf groups (cons (reverse current) groups) current nil)
        (setf current (cons string current))))
    (list
      (parse-ticket-rules (third groups))
      (parse-your-ticket (second groups))
      (parse-nearby-tickets (first groups)))))

(defun rules (doc) (first doc))
(defun your-ticket (doc) (second doc))
(defun nearby-tickets (doc) (third doc))

(defun value-valid-p (rules value)
  (loop for rule in rules for ((from1 . to1) (from2 . to2)) = (cdr rule)
        thereis (or (<= from1 value to1)
                    (<= from2 value to2))))

(defun ticket-errors (rules ticket)
  (loop for value across ticket unless (value-valid-p rules value) sum value))

(defun ticket-scanning-error-rate (doc)
  (reduce #'+ (nearby-tickets doc)
          :key (lambda (ticket) (ticket-errors (rules doc) ticket))))

(defun ticket-valid-p (rules ticket)
  (loop for value across ticket always (value-valid-p rules value)))

(defun rules-to-ticket-positions (rules tickets &aux result)
  (loop for pos below (length rules) do
        (loop for (name (from1 . to1) (from2 . to2)) in rules
              for valid = (loop for ticket in tickets
                                for value = (aref ticket pos)
                                always (or (<= from1 value to1)
                                           (<= from2 value to2)))
              for existing = (assoc name result :test #'string=)
              when valid do
              (if existing
                (push pos (second existing))
                (push (list name (list pos)) result)))
        finally (return result)))

(defun find-mapping (rules tickets)
  (labels ((sort-rules (rules)
             (sort rules #'< :key (lambda (x) (length (second x)))))
           (rules-valid-p (rules)
             (loop for rule in rules for positions = (second rule)
                   always (> (length positions) 0)))
           (update-rules (rules n p)
             (loop for (name positions) in rules
                   when (string/= name n)
                   collect (list name (remove p positions))))
           (recur (rules mapping &aux (rules (sort-rules rules)))
             (cond ((not (rules-valid-p rules)) nil)
                   ((null rules) (return-from find-mapping mapping))
                   (t
                     (loop for (n pp) in rules do
                           (loop for p in pp do
                                 (recur (update-rules rules n p)
                                        (cons (cons n p) mapping))))))))
    (recur (rules-to-ticket-positions rules tickets) nil)))

(define-solution (2020 16) (doc parse-document)
  (values
    (ticket-scanning-error-rate doc)
    (let ((tickets (remove-if-not
                     (lambda (ticket) (ticket-valid-p (rules doc) ticket))
                     (nearby-tickets doc))))
      (loop for (n . p) in (find-mapping (rules doc) tickets)
            when (search "departure" n)
            collect (aref (your-ticket doc) p) into values
            finally (return (reduce #'* values))))))

(define-test (2020 16) (27802 279139880759))
