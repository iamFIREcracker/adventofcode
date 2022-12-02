;;;; Version 0 (the one used for the stars):
;;;; - Ugly group-parsing logic -- I just _hate_ parsing groups
;;;; - [link](https://topaz.github.io/paste/#XQAAAQAZAwAAAAAAAAAd6AQKBhOR2ycIlwrrspIUOH8TNt+SQrY81BDr23O3PL5l+z8+c5p+RT5PaCKaBwc6PsocJaIT6dkRvDFBdg/sKaj1qj0g+edmswo9vvnX7VIO9xQF9qk8mQo3lc7i1+tZW06b0x1p+4waaVboqwvlWubTZ/2o4H4ghACIIGeq1HUHpFffdHdccoX0ahrdBY6iQjFcnq3541iLXNWEmiI08HbAO9Woo+uM/0R97Uf1VuD34QK2jIclJz+FM1kPSYCUmFhWbzoxq/jZ+yCThByuDXc3YO4f2gSZD4jVtt2kDsDaF0H//mnvO9Dy0Lqw2QeyOE6OxPJtnWmwXwgrZzU610CCkgnyWzdiTq+1ju5PWlJST1dr/5eBKAA=)
;;;; Version 1:
;;;; - Smarter group parsing logic: we append "" to the list of strings, that way we don't have to handle between end of group and end of input differently 
;;;; - Also, get rid of some copy-pasta
;;;; - [link](https://topaz.github.io/paste/#XQAAAQDXAQAAAAAAAAAUGQimgx+p6OAGysYQ+iIIWWN8ATAsebvf5nn/Msp1mABzL6XCjbv/fLD94w8odQf37Uv1J71uEB2ce2mJUG8xubh+VNdbUfTpl5BXsYvSM4hoguwCDFIRB2SeeuezwAfW2n//kqso25QnUSVUZtIRqpwxBYr07H/f97dP9zu45n8Q8UzFUsclTaY8wtcHLUytqAAcWXewUKnfv8S0AiCgT9kFrkSnBFD4zHxYFEDK5BrV6ijNgRXM/X/3z+T+DPiicqTZTkZ7PCacHmKI4OYiaPjAidsXH8ZbMLUKL1OdWYRfkM5fEL1Gmgv9osQdaXIcmSS269v8Ajb77oPGRHLCarnfK8/iOSuvXLwwVK+x/5xQtwA=)
;;;; Version 2:
;;;; - Even smarter parsing logic: use SPLIT-SEQUENCE:SPLIT-SEQUENCE and move on with your life
;;;; - Use funky [ reader macro to save us some typing
;;;; - Use REDUCE's :end key argument, instead of SUBSEQ for part 2
;;;; - [link](https://topaz.github.io/paste/#XQAAAQCGAQAAAAAAAAAUGQimgx+p6OAGysYQ+iIIWWN8ATAsebvf5nn/Msp1mABzL6XCjbv/fLD94w8odQf37Uv1J71uEB2ce2mJUG8xubh+VNdbUfTpl5BXsYvSM4hogvKUuavTsIvahznWXgO0V2E9o3amsx6fN2yY6vE7fTcIAbfD/Rx2H9ONR4lqQxpWUmOiiVElME8+KsfAUbeFlhVMJFM1NYkwsy+COYC5iZZ2/NdHnOWfpQAeJBrf5NJAI5JVU5Foj5fBW6TcLc/32/SmEAjIKaa5CSZNjirvawmtaP8rP/4L5WmgZuBY+Zt1QNJT6GJl8L5KjLvTbubhgpv/x6pagA==)

(defpackage :aoc/2022/01 #.cl-user::*aoc-use*)
(in-package :aoc/2022/01)


(defun elf-bags (&optional (strings (uiop:read-file-lines #P"src/2022/day01.txt")))
  (mapcar [mapcar #'parse-integer _]
          (split-sequence:split-sequence "" strings :test #'string=)))

(defun bag-cals (bag) (reduce #'+ bag))

(define-solution (2022 01) (bags elf-bags)
  (values (reduce #'max bags :key #'bag-cals)
          (reduce #'+ (sort (mapcar #'bag-cals bags) #'>) :end 3)))


(define-test (2022 01) (75501 215594))
