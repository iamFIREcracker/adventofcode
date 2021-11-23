(load "main.lisp")

(defmacro test (expected actual)
  `(progn
     (assert (equalp ,expected ,actual))
    t))

;; Day 7-1
(test '("C" "A") (parse-requirement "Step C must be finished before step A can begin."))
(test '("C") (dependency-roots
               '(("C" "A") ("C" "F") ("A" "B") ("A" "D") ("B" "E") ("D" "E") ("F" "E"))
               (second (build-dependency-list '(("C" "A") ("C" "F") ("A" "B") ("A" "D") ("B" "E") ("D" "E") ("F" "E"))))))
(test "CABDFE" (solve-day7-1 '(("C" "A") ("C" "F") ("A" "B") ("A" "D") ("B" "E") ("D" "E") ("F" "E"))))
(test "GRTAHKLQVYWXMUBCZPIJFEDNSO" (day7-1))
;; Day 7-2
(test 15 (solve-day7-2 2 0 '(("C" "A") ("C" "F") ("A" "B") ("A" "D") ("B" "E") ("D" "E") ("F" "E"))))
(test 1115 (day7-2))

;; Day 9-1
(test 32 (solve-day9-1 '(9 25) 1))
(test 8317 (solve-day9-1 '(10 1618) 1))
(test 146373 (solve-day9-1 '(13 7999) 1))
(test 2764 (solve-day9-1 '(17 1104) 1))
(test 54718 (solve-day9-1 '(21 6111) 1))
(test 37305 (solve-day9-1 '(30 5807) 1))
; (test 384892 (day9-1))
;; Day 9-2
;;(test 3169872331 (day9-2))

;; Day13-1
(test '((0 0 1 0 "left")) (parse-carts '(">")))
(test '((0 0 0 1 "left")) (parse-carts '("v")))
(test '((0 0 -1 0 "left")) (parse-carts '("<")))
(test '((0 0 0 -1 "left")) (parse-carts '("^")))
(test  1 (next-vx  0 -1 "left" #\/))
(test  0 (next-vx  1  0 "left" #\/))
(test -1 (next-vx  0  1 "left" #\/))
(test  0 (next-vx -1  0 "left" #\/))
(test -1 (next-vx  0 -1 "left" #\\ ))
(test  0 (next-vx  1  0 "left" #\\ ))
(test  1 (next-vx  0  1 "left" #\\ ))
(test  0 (next-vx  1  0 "left" #\\ ))
(test  0 (next-vx  1  0 "left" #\+))
(test  1 (next-vx  1  0 "straight" '#\+))
(test  0 (next-vx  1  0 "right" '#\+))
(test  1 (next-vx  0  1 "left" '#\+))
(test  0 (next-vx  0  1 "straight" '#\+))
(test -1 (next-vx  0  1 "right" '#\+))
(test "straight" (next-turn "straight" #\|))
(test "right" (next-turn "straight" #\+))
(test '(0 0) (cart-collisions '((0 0) (0 1) (1 0) (0 0))))
(test NIL (cart-collisions '((0 0) (0 1) (1 0) (1 1))))
(test '((0 0 -1 0 "straight")) (move-carts-removing-crashes '("+" "^") '((0 1 0 -1 "left"))))
(test '((0 0 0 -1 "right")) (move-carts-removing-crashes '("+" "^") '((0 1 0 -1 "straight"))))
(test '((0 0 1 0 "left")) (move-carts-removing-crashes '("+" "^") '((0 1 0 -1 "right"))))
(test '((1 0 0 -1 "straight")) (move-carts-removing-crashes '(">+") '((0 0 1 0 "left"))))
(test '((1 0 1 0 "right")) (move-carts-removing-crashes '(">+") '((0 0 1 0 "straight"))))
(test '((1 0 0 1 "left")) (move-carts-removing-crashes '(">+") '((0 0 1 0 "right"))))
(test '((0 1 1 0 "straight")) (move-carts-removing-crashes '("v" "+") '((0 0 0 1 "left"))))
(test '((0 1 0 1 "right")) (move-carts-removing-crashes '("v" "+") '((0 0 0 1 "straight"))))
(test '((0 1 -1 0 "left")) (move-carts-removing-crashes '("v" "+") '((0 0 0 1 "right"))))
(test '((0 0 0 1 "straight")) (move-carts-removing-crashes '("+<") '((1 0 -1 0 "left"))))
(test '((0 0 -1 0 "right")) (move-carts-removing-crashes '("+<") '((1 0 -1 0 "straight"))))
(test '((0 0 0 -1 "left")) (move-carts-removing-crashes '("+<") '((1 0 -1 0 "right"))))
(test '((0 0 1 0 "left")) (move-carts-removing-crashes '("/" "^") '((0 1 0 -1 "left"))))
(test '((0 0 -1 0 "left")) (move-carts-removing-crashes '("\\" "^") '((0 1 0 -1 "left"))))
(test '((1 0 0 1 "left")) (move-carts-removing-crashes '(">\\") '((0 0 1 0 "left"))))
(test '((1 0 0 -1 "left")) (move-carts-removing-crashes '(">/") '((0 0 1 0 "left"))))
(test '((0 1 -1 0 "left")) (move-carts-removing-crashes '("v" "/") '((0 0 0 1 "left"))))
(test '((0 1 1 0 "left")) (move-carts-removing-crashes '("v" "\\") '((0 0 0 1 "left"))))
(test '((0 0 0 1 "left")) (move-carts-removing-crashes '("/<") '((1 0 -1 0 "left"))))
(test '((0 0 0 -1 "left")) (move-carts-removing-crashes '("\\<") '((1 0 -1 0 "left"))))
(test '((0 0 0 -1 "left")) (move-carts-removing-crashes '("\\<") '((1 0 -1 0 "left"))))
(test '((1 0 1 0 "left") (1 0 -1 0 "left")) (move-carts-removing-crashes '("><") '((0 0 1 0 "left") (1 0 -1 0 "left"))))
(test "74,87" (day13-1))
;; Day13-4
(test '((1 0 -1 0 "left")) (move-carts-removing-crashes '("><<") '((0 0 1 0 "left")
                                                                   (1 0 -1 0 "left")
                                                                   (2 0 -1 0 "left"))))
(test "29,74" (day13-2))


;; Day14-1
(test '(0 1) (generate-next-board 0 1 (generate-first-board)))
(test "5158916779" (solve-day14-1 9))
(test "0124515891" (solve-day14-1 5))
(test "9251071085" (solve-day14-1 18))
(test "5941429882" (solve-day14-1 2018))
(test "1191216109" (day14-1))
;; Day14-2
(test 9 (solve-day14-2 "51589"))
(test 5 (solve-day14-2 "01245"))
(test 18 (solve-day14-2 "92510"))
(test 2018 (solve-day14-2 "59414"))
(test 20268576 (day14-2))


;; Day15-1
(defvar *cave*)
(test '(3 1) (progn
               (setf *cave* (parse-goblins-cave (list
                                                  "#######"
                                                  "#E..G.#"
                                                  "#...#.#"
                                                  "#.G.#G#"
                                                  "#######")))
               (cave-find-move-target *cave* (cave-parse-unit *cave* 1 1))))
(test '(4 2) (progn
               (setf *cave* (parse-goblins-cave (list
                                                  "#######"
                                                  "#.E...#"
                                                  "#.....#"
                                                  "#...G.#"
                                                  "#######")))
               (cave-find-move-target *cave* (cave-parse-unit *cave* 2 1))))
(test '(3 1) (progn
               (setf *cave* (parse-goblins-cave (list
                                                  "#######"
                                                  "#.E...#"
                                                  "#.....#"
                                                  "#...G.#"
                                                  "#######")))
               (cave-find-move-target *cave* (cave-parse-unit *cave* 2 1))
               (cave-move-unit *cave*
                               (cave-parse-unit *cave* 2 1)
                               '(4 2))))
(test '(3 1) (progn
               (setf *cave* (parse-goblins-cave (list
                                                  "######"
                                                  "#.G..#"
                                                  "#...E#"
                                                  "#E...#"
                                                  "######")))
               (cave-find-move-target *cave* (cave-parse-unit *cave* 2 1))
               (cave-move-unit *cave*
                               (cave-parse-unit *cave* 2 1)
                               '(4 1))))
(test '(3 1) (progn
               (setf *cave* (parse-goblins-cave (list
                                                  "#######"
                                                  "#.E...#"
                                                  "#..G..#"
                                                  "#.....#"
                                                  "#######")))
               (cave-find-move-target *cave* (cave-parse-unit *cave* 2 1))
               (cave-move-unit *cave*
                               (cave-parse-unit *cave* 2 1)
                               '(3 1))))
(test '(3 1) (progn
               (setf *cave* (parse-goblins-cave (list
                                                  "#######"
                                                  "#.E...#"
                                                  "#.....#"
                                                  "#..G..#"
                                                  "#######")))
               (cave-find-move-target *cave* (cave-parse-unit *cave* 2 1))
               (cave-move-unit *cave*
                               (cave-parse-unit *cave* 2 1)
                               '(3 2))))
(test '(3 2 #\G 3 2) (progn
                       (setf *cave* (parse-goblins-cave (list
                                                           "G...."
                                                           "..G.."
                                                           "..EG."
                                                           "..G.."
                                                           "...G.")))
                       (setf (fifth (cave-parse-unit *cave* 0 0)) 9
                             (fifth (cave-parse-unit *cave* 2 1)) 4
                             (fifth (cave-parse-unit *cave* 3 2)) 2
                             (fifth (cave-parse-unit *cave* 2 3)) 2
                             (fifth (cave-parse-unit *cave* 3 4)) 1)
                       (cave-find-attack-target *cave* (cave-parse-unit *cave* 2 2))))

(test 27730 (progn
              (setf *cave* (parse-goblins-cave (list
                                                 "#######"
                                                 "#.G...#"
                                                 "#...EG#"
                                                 "#.#.#G#"
                                                 "#..G#E#"
                                                 "#.....#"
                                                 "#######")))
              (solve-day15-1 *cave*)))
(test 36334 (progn
              (setf *cave* (parse-goblins-cave (list
                                                 "#######"
                                                 "#G..#E#"
                                                 "#E#E.E#"
                                                 "#G.##.#"
                                                 "#...#E#"
                                                 "#...E.#"
                                                 "#######")))
              (solve-day15-1 *cave*)))
(test 10234 (progn
              (setf *cave* (parse-goblins-cave (list
                                                 "#######"
                                                 "#.E..G#"
                                                 "#.#####"
                                                 "#G#####"
                                                 "#######")))
              (solve-day15-1 *cave*)))
(test 10030 (progn
              (setf *cave* (parse-goblins-cave (list
                                                 "#####"
                                                 "###G#"
                                                 "###.#"
                                                 "#.E.#"
                                                 "#G###"
                                                 "#####")))
              (solve-day15-1 *cave*)))
(test 225096 (day15-1))
(test 35354 (day15-2))



;; Day16-1
(test '(opcode-addi opcode-mulr opcode-seti) (opcodes-matching '(3 2 1 1) '(9 2 1 2) '(3 2 2 1)))
(test '(2 3 2 2) (parse-register "Before: [2, 3, 2, 2]"))
(test '(2 3 2 2) (parse-register "After:  [2, 3, 2, 2]"))
(test 544 (day16-1))
;; Day16-2
(test 600 (day16-2))


;; Day19-1
(test 0 (parse-instruction-pointer "#ip 0"))
(test (list #'opcode-seti '(5 0 1)) (parse-instruction "seti 5 0 1"))
(test 2106 (day19-1))
;; Day19-2
(test 23021280 (day19-2))

;; Day21-1
(test 3909249 (day21-1))
;; Day21-2
; (test 12333799 (day21-2))

;; Day24-1
(test '(18 729 8 "radiation" 10 ("fire") ("slashing" "cold"))
      (parse-group "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"))
(test '(989 1274 25 "slashing" 3 ("slashing" "bludgeoning") ("fire"))
      (parse-group "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3 "))
(test 17542 (day24-1))
;; Day 24-2
(test 868 (day24-2))
