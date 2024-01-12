(defpackage :aoc/2022/10 #.cl-user::*aoc-use*)
(in-package :aoc/2022/10)


(defun parse-program (&optional (strings (uiop:read-file-lines #P"src/2022/day10.txt")))
  (reduce #'nconc (mapcar #'extract-forms strings)))

(define-solution (2022 10) (program parse-program)
  (let ((crt (make-array (list 6 40))) (cycle 1) (x 1) (part1 0))
    (dolist (token program)
      (when (member cycle (list 20 60 100 140 180 220))
        (incf part1 (* cycle x)))
      (update-crt crt cycle x)
      (if (numberp token) ; second cycle of an addx operation
        (incf x token))
      (incf cycle))
    (values part1 (print-crt crt))))

(defun row (cycle) (floor (1- cycle) 40))
(defun col (cycle) (mod (1- cycle) 40))

(defun update-crt (crt cycle x)
  (setf (aref crt (row cycle) (col cycle)) (sprite-visible-p cycle x)))
(defun sprite-visible-p (cycle x) (<= (1- x) (col cycle) (1+ x)))

(defun print-crt (crt)
  (with-output-to-string (s)
    (terpri s) ; print an additional newline, so I can better format the expected string
    (dotimes (i 6)
      (dotimes (j 40)
        (princ (if (aref crt i j) #\# #\Space) s))
      (terpri s))))

(define-test (2022 10) (14220 "
#### ###   ##  ###  #    #### #### #  # 
   # #  # #  # #  # #    #       # #  # 
  #  #  # #  # #  # #    ###    #  #  # 
 #   ###  #### ###  #    #     #   #  # 
#    # #  #  # # #  #    #    #    #  # 
#### #  # #  # #  # #### #    ####  ##  
"))

;;; Version 1:
;;; - Stop parsing lines into strings first and then from strings into lists
;;;   again: instead, simply use UIOP:READ-FILE-FORMS and process one form at
;;;   a time (i.e. one form per cycle).  noop is one token and takes 1 cycle to
;;;   complete; addx requires 2 tokens and takes 2 cycles to complete
;;; - [link](https://topaz.github.io/paste/#XQAAAQC6AwAAAAAAAAAUGQimgx+p6PxKWPPFdcu/6St3K12oLqHOluD0VKSnHL4l9wLpVtil9/FH6o3JcrIt2DV3mN43T5BTumXCpwRCfxmCCVDoPWtDJKB2ley4vbCVdKQCQQ7YXtuctuj2GE+TjYDLWYL3Fla0OxREPzZJgCB/KQHohot7FIycoH5EpIk6R7RefPs5092sjvm1QA+/uokJnVbS43XMgsEam2EgqaDXiDLTSOsHEfLjjoF/zgJ0mFI22nY71fFjG3LE0PrG2iCTCGxe4DkKveKjAiBGPzuQbgRiXS7ZUCmIv+VrL417niTNHotYySb193oIMc78U4HnNDMUHRe/vk9rIgeIC83+nQHJrIQ/hgOa70huVhFlh9jEfOdd/YpteME3TzOesmNY1lqbCaXi5AiVVvcTBF5/Z6ZgEWeAXKl5cY+sHSFCkl9qRkHtjO+HxUVD1JvH7OFqRiTKsKlpuL7Ge2e+TQd4qPZhEASaYkQ7FLsmsjY7hGQumR8UU4LE7PIcpxxbQ4EBqgEc6/WkEnwRasWd95olgFDECoisRpqoCVhd04up96c0Fg9n9NCvUtiOIHLIE/amink2pJTtRp9eBQu/5e1WmfbLsWUFPWtp/1WYMCi+b53PTme8uNcDe/d3QlDYQ9xbKtKCNE4OveNl/8V0tdk=)
;;; Version 0:
;;; - Double implementation for part 1
;;; - [link](https://topaz.github.io/paste/#XQAAAQAXCgAAAAAAAAAd6AQKBhOR2ycIlwrqRiHywFJuxueW2QjzEsdkTii4R/QnV5wGnfTStFBeF0r8xRTCY1ialVrs9oRcKNeVdvwZmNuN1MyeJOoV16zgQSdCXMunEz1DiKE8qWfhtdzAc1udmNO6VUIEhDgxkzbGPRy4V1NMO+1n5Iq7hl2EIhF1upoII0m/2398VS4sW3szLU3lCN2Hie/ahKDqr9hueIu1eSqffe+M2Hu0U+kzLGmWa3cJgF5yUw/cs0xf47Ftd4DJ34p/8m1ZK7qvnDAUbVVqf5czyAzlVnHbpe6nHo7VA+IyKwrqAVbiDSJBU1T2DCUqSF2QHG603QKYcCgAtkt2Ta6PHJh0oKolr74445Z+6PMrmdJPOl5fF0ZTU2lCkdQ7VXKhyx/Su0GnOwBDN0PR1ayrxDNGuYVzl7jAJ8jPF10NIMAK2GEutVdN2geo25VGEOOAACy64s094UDXs70YNZtELiXhVtU5l71PsWvr+QU1UnKnFrmKXAL+09auFmsnkxQo8++rZ9mZd8vlJIWRlMX4PKIBrfx60hFVp5kXefTujcyKrhQjkCrRlm1pjQxzEt6NdH0H0fbEVqt25gncmZzETTLkHZ9bYaFBeT/l+/XFiJWpkc2DVBE2VhExiSC64T5UZWqSPAtsGBS/IndU1DTwH9EVUwq1u6fcKxUESOfACxwMvZnU+maKE1bxgELi1v6Lc/r4L22LNaVB/TQBLkJT4AQVCfD9+uuIeBOmP3RxzauFN9x7cVexahgbaeRjh2DnDFlFXdpR3SyHTk6GCMsDYRNNJLgLbN5pOG4UUdRJ0ilFY+EidymBnGK8gFZYMN2ZyBEYXHQ5N66eWIeXe3mEPymY+AgRALF45KB3rHL6rFSkLKbvVhCulUWpbELqvime/fLP7imiSMERyVWquLtKWE/xR0vUjwhwrJ6m2+i9SXksLmerDR7iZAM2jY6d4kQ8t+xj3v/RvW1BzKOBO//QaqAUfWDsw1yhcbUILkSdrORl2X3fO72lzkO16LjarIRDJZ5tO/LSYyMIrrpkAAML8Vkji6KSRg84HVsFPiCIIMEPwmyfFGa5y+g5cff/xw3Zvg==)
