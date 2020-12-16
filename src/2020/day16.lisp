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

(defun ticket-valid-p (ticket valid-numbers)
  (every (lambda (n) (aref valid-numbers n)) ticket))

(defun ticket-errors (ticket valid-numbers)
  (loop for n across ticket unless (aref valid-numbers n) sum n))

(defun validate-tickets (doc)
  (let ((valid-numbers (make-array 1000 :initial-element nil)))
    (loop for (name . ranges) in (rules doc) do
          (loop for (min . max) in ranges do
                (loop for i from min upto max
                      do (setf (aref valid-numbers i) name))))
    (loop for ticket in (nearby-tickets doc)
          sum (ticket-errors ticket valid-numbers) into total-error-rate
          when (ticket-valid-p ticket valid-numbers) collect ticket into valid-tickets
          finally (return (values valid-tickets
                            total-error-rate)))))

(defun calculate-compatible-fields (rules tickets)
  (let ((result (make-array (length rules) :initial-element nil)))
    (dotimes (pos (length rules) result)
      (loop for (name (from1 . to1) (from2 . to2)) in rules
            for valid = (loop for ticket in tickets for n = (aref ticket pos)
                              always (or (<= from1 n to1) (<= from2 n to2)))
            when valid do (push name (aref result pos))))))

(defun find-field-mapping (compatibles &aux (n (length compatibles)))
  (labels ((recur (mapping &aux (pos (length mapping)))
             (cond
               ((= pos n) (return-from find-field-mapping (reverse mapping)))
               (t (loop for name in (set-difference
                                      (aref compatibles pos)
                                      mapping
                                      :test 'string=) do
                        (recur (cons name mapping)))))))
    (recur nil)))

(defun part2 (doc tickets)
  (let* ((compatibles (calculate-compatible-fields (rules doc) tickets))
         (mapping (find-field-mapping compatibles)))
    (loop for name in mapping for part across (your-ticket doc)
          when (search "departure" name) collect part into numbers
          finally (return (reduce #'* numbers)))))

(define-solution (2020 16) (doc parse-document)
  (multiple-value-bind (valid-tickets error-rate)
      (validate-tickets doc)
    (values error-rate (swallow (part2 doc valid-tickets)))))

(define-test (2020 16) (27802 279139880759))
