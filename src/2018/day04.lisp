(defpackage :aoc/2018/04 #.cl-user::*aoc-use*)
(in-package :aoc/2018/04)

(defun guard (schedule-entry) (car schedule-entry))
(defun sleeptable (schedule-entry) (cdr schedule-entry))
(defun (setf sleeptable) (value schedule-entry) (setf (cdr schedule-entry) value))

(defun minute (sleeptable-entry) (car sleeptable-entry))
(defun days (sleeptable-entry) (cdr sleeptable-entry))
(defun (setf days) (value sleeptable-entry) (setf (cdr sleeptable-entry) value))

(defun parse-schedule (data)
  "Returns an ALIST, with the guard ID as index; the value is another ALIST
  with minute as index and the 'list of days the the guard was found
  asleep at that given time' as value."
  (let (schedule)
    (labels ((parse (remaining)
               (let ((guard (parse-schedule-guard (pop remaining))))
                 (when guard (fill-in-schedule guard remaining))))
             (fill-in-schedule (guard remaining)
               (if (not (parse-schedule-timeday (first remaining)))
                 (parse remaining)
                 (let ((falls-asleep (parse-schedule-timeday (pop remaining)))
                       (wakes-up (parse-schedule-timeday (pop remaining))))
                   (loop :with day = (cdr falls-asleep)
                         :for time :from (car falls-asleep) :below (car wakes-up)
                         :do (add-asleep-time-day guard time day))
                   (fill-in-schedule guard remaining))))
             (add-asleep-time-day (guard time day)
               (let ((entry (assoc guard schedule)))
                 (unless entry
                   (setf entry (list guard))
                   (push entry schedule))
                 (let ((st-entry (assoc time (sleeptable entry))))
                   (unless st-entry
                     (setf st-entry (list time))
                     (push st-entry (sleeptable entry)))
                   (push day (days st-entry))))))
      (parse (sort (copy-seq data) #'string<)))
    schedule))

(defun parse-schedule-guard (string)
  (cl-ppcre:register-groups-bind ((#'parse-integer guard))
      ("Guard #(\\d+) begins shift" string)
    guard))

(defun parse-schedule-timeday (string)
  (cl-ppcre:register-groups-bind (date (#'parse-integer time))
      ("(\\d{4}-\\d{2}-\\d{2}) 00:(\\d\\d)] (falls asleep|wakes up)" string)
    (cons time date)))


(defun part1 (schedule)
  (let ((entry (find-max schedule :key #'minutes-asleep)))
    (* (guard entry) (sleepiest-minute entry))))

(defun minutes-asleep (entry)
  (reduce #'+ (sleeptable entry)
          :key #'(lambda (e) (length (cdr e)))))

(defun sleepiest-minute (entry)
  (multiple-value-bind (st num-days-asleep)
      (find-max (sleeptable entry)
                :key #'(lambda (st)
                        (length (days st))))
    (values (minute st) num-days-asleep)))


(defun part2 (schedule)
  (let ((entry (find-max schedule
                         :key #'(lambda (entry)
                                 (nth-value 1 (sleepiest-minute entry))))))
    (* (guard entry) (sleepiest-minute entry))))


(define-solution (2018 4) (schedule parse-schedule)
  (values (part1 schedule) (part2 schedule)))

(define-test (2018 4) (3212 4966))
