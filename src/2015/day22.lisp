(defpackage :aoc/2015/22 #.cl-user::*aoc-use*)
(in-package :aoc/2015/22)

(defstruct (spell (:type list)) name cost damage heal effect)

(defparameter *spells* '(("Magic Missile" 53  4 0)
                         ("Drain"         73  2 2)
                         ("Shield"        113 0 0 (6 0   0 7))
                         ("Poison"        173 0 0 (6 0   3 0))
                         ("Recharge"      229 0 0 (5 101 0 0))))

(defstruct (effect (:type list)) turns mana damage armor)

;; Player logic
(defstruct (player (:type list)) life mana effects)

(defun player-armor-effect (player)
  (reduce #'+ (player-effects player) :key #'effect-armor))

(defun player-mana-effect (player)
  (reduce #'+ (player-effects player) :key #'effect-mana))

(defun player-damage-effect (player)
  (reduce #'+ (player-effects player) :key #'effect-damage))

(defun player-total-mana (player)
  (+ (player-mana-effect player)
     (player-mana player)))

(defun player-total-damage (player spell)
  (+ (player-damage-effect player) (spell-damage spell)))

(defun life-next (life spell-heal damage) (- (+ life spell-heal) damage))

(defun mana-next (mana spell-cost) (- mana spell-cost))

(defun effects-next (effects spell-effect)
  (let ((effects (loop for (turns . rest) in effects
                       when (> (1- turns) 0) collect (cons (1- turns) rest))))
    (if spell-effect
      (cons spell-effect effects)
      effects)))

(defun player-next (player damage
                           &optional (spell  '("Dummy" 0 0 0 nil))
                           &aux (player (copy-seq player)))
  (setf (player-life player) (life-next (player-life player)
                                        (spell-heal spell)
                                        damage)
        (player-mana player) (mana-next (player-total-mana player)
                                        (spell-cost spell))
        (player-effects player) (effects-next (player-effects player)
                                              (spell-effect spell)))
  player)

;; Boss logic
(defstruct (boss (:type list)) life damage)

(defun parse-boss (&optional (lines (uiop:read-file-lines #P"src/2015/day22.txt")))
  (extract-positive-integers (format nil "~{~A ~}" lines)))

(defun boss-attack (boss player)
  (max 1 (- (boss-damage boss) (player-armor-effect player))))

(defun boss-next (boss damage)
  (let ((boss (copy-seq boss)))
    (setf (boss-life boss) (- (boss-life boss) damage))
    boss))

;; State

(defstruct (state (:type list) (:conc-name)) player boss)

(defun not-enough-mana-p (mana spell) (> (spell-cost spell) mana))

(defun effect-in-use-p (effects spell)
  (loop for (turns . rest) in effects
        thereis (and (equal (rest (spell-effect spell)) rest)
                     (> turns 1))))

(defun castable-spells (mana effects)
  (loop for spell in *spells*
        unless (or (not-enough-mana-p mana spell)
                   (effect-in-use-p effects spell))
        collect spell))

(defun player-turn (state spell
                           &aux (player (player state)) (boss (boss state)))
  (let ((boss-damage (player-total-damage player spell)))
    (cons (list (player-next player 0 spell) (boss-next boss boss-damage))
          (spell-cost spell))))

(defun all-player-turns (state &aux (player (player state)))
  (loop for spell in (castable-spells (player-total-mana player) (player-effects player))
        collect (player-turn state spell)))

(defun boss-turn (state)
  (let ((player (player state)) (boss (boss state)))
    (let ((boss-damage (player-damage-effect player))
          (player-damage 0))
      (when (< boss-damage (boss-life boss))
        (setf player-damage (boss-attack boss player)))
      (list (player-next player player-damage) (boss-next boss boss-damage)))))

(defun player-alive-p (player) (> (player-life player) 0))

(defun play (state)
  (loop for (next . cost) in (all-player-turns state)
        for next-next = (boss-turn next)
        when (player-alive-p (player next-next))
        collect (cons next-next cost)))

(defun boss-alive-p (boss) (> (boss-life boss) 0))
(defun player-wins-p (state) (not (boss-alive-p (boss state))))

(defun part1 (player boss &aux (best 10000))
  (labels ((recur (state cost-so-far)
             (cond ((player-wins-p state) (setf best (min best cost-so-far)))
                   ((>= cost-so-far best) nil)
                   (t
                     (loop for (next . cost) in (play state)
                           do (recur next (+ cost-so-far cost)))))))
    (recur (list player boss) 0)
    best))

(defun life-is-hard (state &aux (state (copy-seq state)))
  (setf (player state) (copy-seq (player state)))
  (decf (player-life (player state)))
  state)

(defun play-hard (state &aux (state (life-is-hard state)))
  (when (player-alive-p (player state))
    (play state)))

(defun part2 (player boss &aux (best 10000))
  (labels ((recur (state cost-so-far)
             (cond ((player-wins-p state) (setf best (min best cost-so-far)))
                   ((>= cost-so-far best) nil)
                   (t
                     (loop for (next . cost) in (play-hard state)
                           do (recur next (+ cost-so-far cost)))))))
    (recur (list player boss) 0)
    best))

(define-solution (2015 22) (boss parse-boss)
  (values (part1 (list 50 500 nil) boss) (part2 (list 50 500 nil) boss)))

(define-test (2015 22) (900 1216))
