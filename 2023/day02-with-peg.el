;;; Advent of Code 2023
;;; Saturday, December 02, 2023

;;; A different way to solve day 2.  Written after the initial
;;; submission to understand the peg grammars.

(require 'peg)

(defun day2-parse-game ()
  "parse the game at the current point"
  (with-peg-rules
      ((game (bol) "Game" space number ":" (+ roll) (eol))
       (number (substring (+ digit))
	       `(n -- (string-to-number n)))
       (digit (range ?0 ?9))
       (space (syntax-class whitespace))
       (roll (list (+ cube)) (or ";" (eol)))
	      (color (substring (or "red" "blue" "green")))
	      (cube space number space color (opt ",")
		    `(n c -- (cons n c))))
    (reverse (peg-run (peg game)))))

(defun day2-possible-p (pair)
  "validate PAIR values"
  (cond ((equal "blue" (cdr pair)) (<= (car pair) 14))
	((equal "green" (cdr pair)) (<= (car pair) 13))
	((equal "red" (cdr pair)) (<= (car pair) 12))))

(defun day2-check-game (game)
  (let ((valid t))
    (dolist (round (cdr game) valid)
      (dolist (cube round)
	(unless (day2-possible-p cube)
	  (setf valid nil))))))

(defun day2-part-1 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((solution 0)
	  (game (day2-parse-game)))
      (while game
	(when (day2-check-game  game)
	  (cl-incf solution (car game)))
	(forward-line)
	(setq game (day2-parse-game)))
      solution)))

(defun day2-min-cubes (game)
  (let ((red 1)
	(green 1)
	(blue 1))
    (dolist (round (cdr game) (* red green blue))
      (dolist (cube round)
	(cond ((equal "red" (cdr cube)) (setf red (max red (car cube))))
	      ((equal "blue" (cdr cube)) (setf blue (max blue (car cube))))
	      ((equal "green" (cdr cube)) (setf green (max green (car cube)))))))))

(defun day2-part-2 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((solution 0)
	  (game (day2-parse-game)))
      (while game
	(cl-incf solution (day2-min-cubes game))
	(forward-line)
	(setq game (day2-parse-game)))
      solution)))

;; (aoc-copy-output () (day2-part-1 "day2.2023.input.txt"))
;; (aoc-copy-output () (day2-part-2 "day2.2023.input.txt"))
