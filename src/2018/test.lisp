(load "main.lisp")

(defmacro test (expected actual)
  `(progn
     (assert (equalp ,expected ,actual))
    t))


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
