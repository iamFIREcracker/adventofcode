(defpackage :aoc/2016/11a #.cl-user::*aoc-use*)
(in-package :aoc/2016/11a)
#;
(setf *elements-mapping*
  '(("strontium" 1)
    ("plutonium" 2)
    ("thulium" 4)
    ("ruthenium" 8)
    ("curium" 16)
    ("elerium" 32)
    ("dilithium" 64)))

(defparameter *all-elements* nil)
(defparameter *all-elements-mask* nil)
(defparameter *microchips-mask* nil)

(defstruct (state :conc-name) elevator floors)

(defun floor-generators (floor)
  (ash floor (- *all-elements*)))

(defun floor-microchips (floor)
  (logand *microchips-mask* floor))

(defun parse-generators (string)
  (mapcar (lambda (each)
            (let ((name (first (split-sequence:split-sequence #\Space each))))
              (cadr (assoc name *elements-mapping* :test #'string=))))
          (cl-ppcre:all-matches-as-strings "\\w+ generator" string)))

(defun parse-microchips (string)
  (mapcar (lambda (each)
            (let ((name (first (split-sequence:split-sequence #\- each))))
              (cadr (assoc name *elements-mapping* :test #'string=))))
          (cl-ppcre:all-matches-as-strings "\\w+-compatible" string)))

(defun parse-floor (string)
  (let ((gens (loop :for each :in (parse-generators string)
                    :sum each))
        (ucs (loop :for each :in (parse-microchips string)
                   :sum each)))
    (+ (ash gens *all-elements*)
       ucs)))

(defun parse-state (data)
  (make-state :elevator 0
              :floors (make-array 4 :initial-contents (loop :for string :in data
                                                            :collect (parse-floor string)))))

(defun nothing-fries-p (floor)
  (let ((generators (floor-generators floor))
        (microchips (floor-microchips floor)))
    (or (zerop generators)
        (= (logand microchips generators) microchips))))

(defun all-items-at-fourth-p (state)
  (let* ((floor (aref (floors state) 3)))
    (= floor *all-elements-mask*)))

(defun floor-without (floor index index2)
  (logandc2 floor (logior (ash 1 index)
                          (ash 1 index2))))

(defun floor-with (floor index index2)
  (logior floor (ash 1 index) (ash 1 index2)))

(defun change (floors floor-id floor floor2-id floor2)
  (let ((copy (copy-seq floors)))
    (setf (aref copy floor-id) floor
          (aref copy floor2-id) floor2)
    copy))

(defun/profiled neighbors (state)
  (with-slots (elevator floors) state
    (loop :with floor = (aref floors elevator)
          :for index :below (integer-length floor)
          :when (logbitp index floor)
          :append (loop :for index2 :from index :below (integer-length floor) ;; index2 starts from index, that's how we try to move a single item
                        :when (and (logbitp index2 floor)
                                   (nothing-fries-p (floor-without floor index index2)))
                        :append (loop :for dir :in (list -1 1)
                                      :for elevator2 = (+ elevator dir)
                                      :for floor2 = (and (<= 0 elevator2 3) (aref floors elevator2))
                                      :when (and floor2 (nothing-fries-p (floor-with floor2 index index2)))
                                      :collect (make-state :elevator elevator2
                                                           :floors (change floors
                                                                           elevator
                                                                           (floor-without floor index index2)
                                                                           elevator2
                                                                           (floor-with floor2 index index2))))))))
(defun/profiled element-pairs (floors)
  (loop for index below *all-elements*
          for pos-micro = (find-if (partial-1 #'logbitp index) (list 0 1 2 3)
                                   :key (partial-1 #'floor-microchips (aref floors _)))
          for pos-gen = (find-if (partial-1 #'logbitp index) (list 0 1 2 3)
                                   :key (partial-1 #'floor-generators (aref floors _)))
          collect (list pos-micro pos-gen)))

(defun/profiled micro-first-gen-later (elem1 elem2)
  (or (< (first elem1) (first elem2))
      (and (= (first elem1) (first elem2))
           (< (second elem1) (second elem2)))))

(defun/profiled sort-pairs (pairs)
  (sort pairs #'micro-first-gen-later))

(defun/profiled state-key (state)
  (with-slots (elevator floors) state
    (let ((pairs (element-pairs floors)))
      (list elevator (sort-pairs pairs)))))

(defun solve (data &optional part2)
  (let* ((*all-elements* (if part2 7 5))
         (*all-elements-mask* (1- (ash 1 (* *all-elements* 2))))
         (*microchips-mask* (1- (ash 1 *all-elements*)))
         (seen (make-hash-table :test 'equal)))
    (nth-value 3 (bfs (parse-state data)
                      :test 'equalp
                      :goalp #'all-items-at-fourth-p
                      :neighbors #'neighbors
                      :prunep (lambda (state cost)
                                (declare (ignore cost))
                                (let ((key (state-key state)))
                                  (prog1 (gethash key seen)
                                    (setf (gethash key seen) t))))))))
    ; (nth-value 3 (bfs (parse-state data)
    ;                   :test 'equalp
    ;                   :goalp #'all-items-at-fourth-p
    ;                   :neighbors #'neighbors))))

(defun prepare-part2 (data)
  (let ((copy (copy-seq data)))
    (setf (nth 0 copy) (format nil "~A ~{~A ~}" (nth 0 copy)
                               (list "elerium generator"
                                     "elerium-compatible microchip"
                                     "dilithium generator"
                                     "dilithium-compatible microchip")))
    copy))
