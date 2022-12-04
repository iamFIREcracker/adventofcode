(defpackage :aoc/2022/04 #.cl-user::*aoc-use*)
(in-package :aoc/2022/04)


(defun assignment-pairs ()
  (mapcar [mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" _)]
          (uiop:read-file-lines #P"src/2022/day04.txt")))

(defun solution-run ()
  (loop for (a b c d) in (assignment-pairs)
        count (or (<= a c d b) (<= c a b d)) into part1
        count (or (<= a c b d) (<= a c d b)
                  (<= c a d b) (<= c a b d)) into part2
        finally (return (values part1 part2))))

(define-test (2022 04) (569 936))

;;; Version 1 (this):
;;; - Use CL-PPCRE:ALL-MATCHES-AS-STRINGS instead of CL-PPCRE:DO-REGISTER-GROUPS 
;;; - Merge the two loops into one
;;; - [link](https://topaz.github.io/paste/#XQAAAQCyAQAAAAAAAAAUGQimgx+p6NeA2AwBIArz/HBdclF3hRZev4uqus4U7xlOrEvMl4zyK5yvj0mMZbeOJYzrcga5Kk29BYV+lPdzxbKIFcr4NY9yIYA9we1UpxHa68LWFh8mseWT2JLkh3vr0bX67m2azRbZOWBB6Nv0yJipAr6tBmPVZprgTWD2lWrFLiwBNJWGhpHCwpAyohfbD4lrsrTy78vaNQtBJ2O0iTMDyurx1PDAk1DSsvU/qBOyI/ixwOR29ALr2cHiJbTRp1DjgfKP6m5PwkJwxCbuPsB9VYBsNTiYRJbfsVAjoXb692uBYJ1b8H8BpXCvn3T7FLRcFtAr+/LxH45wZT7eG/2Ht0Q=)
;;; Version 0:
;;; - Not to bad as is
;;; - [link](https://topaz.github.io/paste/#XQAAAQCfAQAAAAAAAAAUG0gnM31Tanr+2XFUdVjkXruVgqGfmNzdDUKUWZvxIR+lgjFaJ6g2/w2R0MAf8LAHR5xMDghoNC2Zp3es/poocUNnuLp0bTLGRNAUXu59t4kczMp6IJweAv3LifI54nv6s1WpgmiHOuNvlVObd1DjW9KvRNviJOif39zgY8kWV0YillelgZoXpWV3lBv0z3VZc+SnojLxw/4gP03+M4/UiEcMGz5fWPfwSvXK9x672VsaVkPEq5tMPP9jwxnzVoP7IHeTlyzPDWhm02wfzGK1OdyuU///ortKoA==)
