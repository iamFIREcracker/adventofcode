(defpackage :aoc/2020/21 #.cl-user::*aoc-use*)
(in-package :aoc/2020/21)

(defun parse-ingredients (string)
  (cl-ppcre:split " " string))

(defun parse-allergens (string)
  (cl-ppcre:split ", " string))

(defun parse-food (string)
  (cl-ppcre:register-groups-bind ((#'parse-ingredients ingredients)
                                  (#'parse-allergens allergens))
      ("(.*) \\(contains (.*)\\)" string)
    (list ingredients allergens)))
(defun ingredients (food) (first food))
(defun allergens (food) (second food))

(defun parse-foods (data)
  (mapcar #'parse-food data))

(defun find-mapping (foods)
  (labels ((sort-foods (foods)
             (sort foods
                   (lambda (food1 food2)
                     (or (< (length (ingredients food1))
                            (length (ingredients food2)))
                         (and (= (length (ingredients food1))
                                 (length (ingredients food2)))
                              (< (length (allergens food1))
                                 (length (allergens food2))))))))
           (guess (foods i a)
             (loop for (ii aa) in foods
                   for invalidp = (and (member a aa :test #'string=)
                                       (not (member i ii :test #'string=)))
                   if invalidp return nil
                   else collect (list
                                  (remove i ii :test #'string=)
                                  (remove a aa :test #'string=))))
           (recur (foods mapping &aux (foods (sort-foods foods)))
             (cond ((every #'null (mapcar #'allergens foods))
                    (return-from find-mapping (values mapping foods)))
                   (t
                     (loop for (ii aa) in foods do
                           (loop for i in ii do
                                 (loop for a in aa
                                       for foods-next = (guess foods i a)
                                       when foods-next do
                                       (recur foods-next
                                              (cons (cons i a) mapping)))))))))
    (recur foods nil)))

(define-solution (2020 21) (foods parse-foods)
  (multiple-value-bind (mapping remaining) (find-mapping foods)
    (values (reduce #'+ (mapcar #'ingredients remaining) :key #'length)
            (format nil "~{~a~^,~}"
                    (mapcar #'first (sort mapping #'string< :key #'cdr))))))

(define-test (2020 21) (1958 "xxscc,mjmqst,gzxnc,vvqj,trnnvn,gbcjqbm,dllbjr,nckqzsg"))
