(defpackage :aoc/2020/04 #.cl-user::*aoc-use*)
(in-package :aoc/2020/04)

(defun parse-line (string)
  (loop for part in (split-sequence:split-sequence #\Space string)
        for (name value) = (split-sequence:split-sequence #\: part)
        unless (string= name "cid")
        append (list (as-keyword name) value)))

(defun parse-passports (data)
  (let (passports current)
    (dolist (string (append data '("")) passports)
      (if (string= string "")
        (setf passports (cons current passports) current nil)
        (setf current (append current (parse-line string)))))))

(defun has-all-required-fields-p (passport)
   (= (length passport) 14)) ; it's a plist, and we skipped the "cid" field

(defun in-range-p (string left right)
  (<= left (parse-integer string) right))

(defun valid-height-p (string)
  (let ((number (subseq string 0 (- (length string) 2)))
        (unit (subseq string (- (length string) 2))))
    (cond ((string= unit "cm") (in-range-p number 150 193))
          ((string= unit "in") (in-range-p number 59 76)))))

(defun valid-hex-color-p (string)
  (and (char= (char string 0) #\#)
       (parse-integer (subseq string 1) :radix 16)))

(defun valid-color-name-p (string)
  (member string '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")
          :test #'string=))

(defun valid-passportid-p (string)
  (and (= (length string) 9) (parse-integer string)))

(defun field-valid-p (name value)
  (handler-case (case name
                  (:byr (in-range-p value 1920 2002))
                  (:iyr (in-range-p value 2010 2020))
                  (:eyr (in-range-p value 2020 2030))
                  (:hgt (valid-height-p value))
                  (:hcl (valid-hex-color-p value))
                  (:ecl (valid-color-name-p value))
                  (:pid (valid-passportid-p value)))
    (error () nil)))

(defun all-fields-valid-p (passport)
  (loop for (name value) on passport by #'cddr
        always (field-valid-p name value)))

(define-solution (2020 4) (data)
  (let ((passports (remove-if-not #'has-all-required-fields-p
                                  (parse-passports data))))
    (values
      (length passports)
      (count-if #'all-fields-valid-p passports))))

(define-test (2020 4) (250 158))
