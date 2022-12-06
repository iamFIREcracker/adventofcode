(defpackage :aoc/2022/06 #.cl-user::*aoc-use*)
(in-package :aoc/2022/06)

(defun marker-position (n)
  (destructuring-bind (left right)
      (split (coerce (uiop:read-file-line #P"src/2022/day06.txt") 'list) (1- n))
    (setf left (reverse left))
    (loop for i from n for ch in right for test = (cons ch (subseq left 0 (1- n)))
          if (< (length (remove-duplicates test)) n) do (push ch left)
          else return i)))

(defun solution-run ()
  (values (marker-position 4) (marker-position 14)))

(define-test (2022 06) (1702 3559))

;;; Version 1 (this):
;;; - Create MARKER-POSITION function to avoid off-by-one errors
;;; - [link](https://topaz.github.io/paste/#XQAAAQCwAQAAAAAAAAAUGQimgx+p6PBiO/jEWNnvSpsZHDy0lJiGnyuj1TbTA5ny0vJnrUm1I0wQkosoAfW6Z0a4i1q5qPryPi6BK4KENzwx/soAxFL8OhgjCMCMoyAwND/euXSxhIqQrjm7zOLRjvQ2W3uiMHmUoFaesJKa+njlKHozChQd0VJUgdlEatdubiVUu3eGrQcO2HRRoWcaDh5bOLrUmMsXLi7bAu4wQo3rZo0BiqJVJjcWdJOsP9X/ci9KYlpRWnlh2VFUmxajvC5SLjPl9Nkj0/2Rt5we1Xp2ZqXberui4catiQx8r32/Jxti5U+cbtsiWZ4sJu4w0VtE8nFJnF1lFiBt6CsmvpwtR8C3BlqfsnXh+6b1o+5Y3Pb2FjeHFyHp+rj0/A==)
;;; Version 0:
;;; - Use a stack to keep track of all the processed characters so far
;;; - If the next char plus the top 3/13 elements seen so far, contain duplicates, then move on
;;; - Otherwise, we have found ourselves our marker
;;; - [link](https://topaz.github.io/paste/#XQAAAQBwAQAAAAAAAAAd6AQKBhOR2ycJw7NgZEsFdfmIe6/LWKLqH+F+Lml0Uu1BeAKYfWc45vVXpodMyq8ukK8Ksg4w453Va+BrXLLGULGBVTHsuAto3oXnepJxYREfzsPt+onGtOI717jRw/dGmS7cRX/zEYefTeN1xjd2UIfN6/Zid7Vx7OU/iG3JnjvmnApXjeEkWHC1CvDfzpcjqY7Xf9VKcRJmD3+VAczPpF3kN8YcaXMOmHq0L/b4Q85M+oW9d4HzPwFosMEcuxtU461Gm2g4/BVhFsn8GXLnWST9lLZCAk6dBVrr1pmhOesvrW8hq9eUeeJKdPtNIUOIwjRAyvE2O96ci0DZGIf/0uMA4CEOBBZxaABisZIn//O3rYc=)
