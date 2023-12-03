;;; Advent of Code 2023
;;; day2 using Parsing Expression Grammars (PEG)
;;; Saturday, December 02, 2023

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
  (cond ((equal "blue" (cdr pair)) (= 14 (car pair)))
	((equal "green" (cdr pair)) (= 13 (car pair)))
	((equal "red" (cdr pair)) (= 12 (car pair)))))

(defun day2-check-game (game)
  (if (cl-every #'identity
		(mapcan (lambda (round) (mapcar #'day2-possible-p round))
			(cdr game)))
      (car game)
    0))

(with-current-buffer "day2.2023.input.txt"
  (beginning-of-buffer)
  (let ((solution 0)
	(game (day2-parse-game)))
    (while game
      (cl-incf solution (day2-check-game game))
      (forward-line)
      (setq game (day2-parse-game)))
    solution))



