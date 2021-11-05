(defpackage :aoc/2018/03 #.cl-user::*aoc-use*)
(in-package :aoc/2018/03)

(defstruct claim id left top right bottom)

(defun parse-claim (str)
  (cl-ppcre:register-groups-bind (id (#'parse-integer left top width height))
      ("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" str)
    (make-claim :id id
                :left left
                :top top
                :right (+ left width)
                :bottom (+ top height))))

(defun make-fabric (claims)
  (let ((fabric (make-hash-table :test 'equal)))
    (dolist (claim claims fabric)
      (dorange (x (claim-left claim) (claim-right claim))
        (dorange (y (claim-top claim) (claim-bottom claim))
          (push
            (claim-id claim)
            (gethash (mkstrc x y) fabric)))))))

(define-solution (2018 3) (data)
  (let* ((claims (mapcar #'parse-claim data))
         (fabric (make-fabric claims)))
    (values
      (loop
        :for overlapping :being :the :hash-value :of fabric
        :counting (> (length overlapping) 1))
      (let ((overlapping-ids (make-hset '() :test 'equal)))
        (loop
          :for ids :being :the :hash-value :of fabric
          :do (when (> (length ids) 1)
                (dolist (id ids)
                  (hset-add id overlapping-ids))))
        (dolist (claim claims)
          (unless (gethash (claim-id claim) overlapping-ids)
            (return (claim-id claim))))))))

(define-test (2018 3) (107820 "661"))
