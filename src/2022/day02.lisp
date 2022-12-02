;;;; Version 0 (the one used for the stars):
;;;; - Kept track not only of our score, but also the opponent's one (it was not required...)
;;;; - For part 2, I ended up changing both the parsing and the looping logic (it's not required...see later)
;;;; - [link](https://topaz.github.io/paste/#XQAAAQBWBwAAAAAAAAAd6AQKBhOR2ycIlwrqRiHywFJuxueW2QjzG+6/cZGkvU+2/1gn5WaRbwV6tIqysLuUZax8ticZmlBd5xQecpRPBgOiFTBwXnNx0FVsYvqVRQ5yHmUjD/PhdNzwUAvmypXUDyFwhRYfDmFjQ0ZCGKfHwzj0S6RN/ti0rvP8xhrQ2Yk4VcGTqEB3ptRC3xLXeNe70FXOfHrHUF4L0AdD3pzBKTc/f5Lpc2Eon5STrjYwZeSVvFjZWx991TlXDi4D/P+Cn67h2su3Ii0NwQZZhOXV4kvcZLt3Ao2Q3266mQhMEHFA4DN9259VMmIeVvmkBZ9jOEmje5M5/p2kMrbt6APSDAgVm/D78qBJ0p38XTTwqHTxVLuFJqJWMskOgdLXnPE66w7FtMBTOIGBpFF2n7ctMoQTkkccoMsZsLnOZkNDtCBgI9r0TDgtRzhc6l4uadx6hJsy5M7CVwy+G0c4pr3pIxjQ0/MbPqLudzBrdK8P+8qS41PEi83gucp3u0gw6EY6ZI86CKnYLFhEIegwYJ+h11tHh+YMXX1yOMi0qKgUGTLGvW06NvSPxkvnYeSgyEN+lpsesv1suLXjvkilDXyj/UW+dBKN7oNSWv4acuk=)
;;;; Version 1:
;;;; - Kept the looping logic the same, and only tweaked the parsing logic
;;;; - Only _your_ socre is required, and if we don't have to store two scores anymore, then the LOOP form can be simplified a lot
;;;; - [link](https://topaz.github.io/paste/#XQAAAQAkBAAAAAAAAAAd6AQKBhOR2ycIlwrqRiHywFJuxueW2QjzG+6/cZGkvU+2/1gn5WaRbwV6tIqysLuUZax8ticZmlBd5xQecpRPBgOiFTBwXnNx0FVsYvqVRQ5yHmUjD/PhdNzwUAvmypXUDyFwhRYfDmFjQ0ZCGKfHwzj0S6RN/ti0rvP8xhrQ2Yk4VcGTqEB3ptRC3xLXeNe70FXOfHrHUF4L0AdD3pzBKTc/f5Lpc2Eon5STumY7DqUQ9UU3R3MmGlwEevcUAtTSzeB8e7FQJ7Rmgo0Oflk9bO+OlJ/oXo1D007nXJoLRyXISkYXvHpgrCQ/lLF9gGvKqCz8ns0UKZRb5o1oT2XoJT/byUspk5T0BqOi9P2yyayXUWxuzDQt5F55wWZzCFE6bdfCbMDINJlZc0R4+ctorm7ppNMOCON2PHO0opedRZxuaVI6CugAvyhDxS6QegwImrfmv8BO/t4q+lu4dp1m4j5ih9MRW+VM9ygFHB/74e4opnzfMGMB7KpDeJLKn6x2MP72qpOc)
;;;; Version 2:
;;;; - Why use UIOP:READ-FILE-LINES while we could have just used UIOP:READ-FILE-FORMS instead?
;;;; - We can use MEMBER instead ECASE + EQ inside WIN?
;;;; - Don't need WINS-TO anymore: (LOSES-TO x) == (WINS-TO (WINS-TO X))
;;;; - [link](https://topaz.github.io/paste/#XQAAAQDSBAAAAAAAAAAd6AQKBhOR2ycIlwrqRiHywFJuxueW2QjzG+6/dvsDjKKBs/yajlDlKwdE3HZ5XqEwKjWDdug73rNDtcLpzyh6ug82TWELItKFzcd6gPKBvzLPvzLOyhqXYpuMf3KAS5svwDw5oc3VqivaMfvIWrkaBmAwLNKvmL0dip556ixOS7ZHbNz1cCJJpEX60aJ9yuSwvZ8JizlFe3ciiGcaypwBztqAT4r+0+bgYuspC+TAXhpXIZGFqx6mXLwClGYOdZjXFM8JgMVBP70zVurOUKd3hd7/E0KYxJpjowt+ZKi1PqU7p51t5TBxCGKmQGuNvBq9R2xCA1Ozjtn9Q4plKMYuRxVPmWI9ITFmlmyoQGxWfWGdJLeVsFKOYsel9shmWxZ6WMLblL0WSrmMPv2TWJ/PIkdFSqpTUV5DgihXVcf6qpU+nlaWAhv2+UhFnuJA8WtB1y+b9n2hjVJpGTo7BtUv2DEt+W9cXQukm2hb61lgJ41nTTRlU6QCvhe+2F1rEJTOwH5m0WfXfq55if3veCnjb0C9Mnzb5nATHoxhgp+r4fcgX4uHUkQFWfsQEOSD6oEi7hT13zeggWWVaXe1JgEkltBfjMUOXLz/7w6Bvg==)

(defpackage :aoc/2022/02 #.cl-user::*aoc-use*)
(in-package :aoc/2022/02)

(defun parse-moves ()
  (flet ((move (v) (ecase v ((a x) 'r) ((b y) 'p) ((c z) 's))))
    (loop for (them you) on (uiop:read-file-forms #P"src/2022/day02.txt") by #'cddr
          collect (cons (move them) (move you)))))

(defun win? (you them) (member (list you them) '((r s) (p r) (s p)) :test 'equal))

(defun move-score (you) (ecase you (r 1) (p 2) (s 3)))

(defun solve (moves)
  (loop for (them . you) in moves
        if (win? them you) sum (+ (move-score you))
        else if (win? you them) sum (+ (move-score you) 6)
        else sum (+ (move-score you) 3)))

(defun parse-moves2 ()
  (flet ((move (v) (ecase v (a 'r) (b 'p) (c 's))))
    (loop for (them you) on (uiop:read-file-forms #P"src/2022/day02.txt") by #'cddr
          collect (let ((them (move them)))
                    (cons
                      them
                      (ecase you
                        (x (loses-to them))
                        (y them)
                        ;; (LOSES-TO (LOSES-TO X)) === (WINS-P X)
                        (z (loses-to (loses-to them)))))))))

(defun loses-to (you) (ecase you (r 's) (p 'r) (s 'p)))

(defun solution-run ()
  (values (solve (parse-moves))
          (solve (parse-moves2))))


(define-test (2022 02) (14297 10498))
