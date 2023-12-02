;;; Day 2: Cube Conundrum
;;; Advent of Code 2023
;;; Saturday, December 02, 2023

;;       -------Part 1--------   -------Part 2--------
;; Day       Time  Rank  Score       Time  Rank  Score
;;   2   00:38:17  8527      0   00:48:41  8508      0

;;; part 1

(defun day2-to-rgb (cubes)
  (let ((red 0)
	(green 0)
	(blue 0))
    (dolist (color (string-split cubes ","))
      (cond ((string-match "blue" color)
	     (cl-incf blue (string-to-number color)))
	    ((string-match "red" color)
	     (cl-incf red (string-to-number color)))
	    ((string-match "green" color)
	     (cl-incf green (string-to-number color)))))
    (list red green blue)))

(defun day2-possible-p (cubes)
  (cl-destructuring-bind (r g b) (day2-to-rgb cubes)
    (and (<= 0 r 12)
	 (<= 0 g 13)
	 (<= 0 b 14))))

(defun day2-game-id (game)
  (let ((tokens (string-split game)))
    (string-to-number (cadr tokens))))

(defun day2-part1 (input-buffer)
  (let ((solution 0))
    (dolist (line (aoc-buffer-lines input-buffer) solution)
      (let ((game (string-split line "[:;]")))
	(when (cl-every #'day2-possible-p (cdr game))
	  (cl-incf solution (day2-game-id (car game))))))))

;; (aoc-copy-output () (day2-part1 "day2.2023.input.txt"))
;; (aoc-copy-output () (day2-part1 "test.buff"))

;;; part 2

(defun day2-fewest (cubes)
  (let ((red 0)
	(blue 0)
	(green 0))
    (dolist (one-pull cubes)
      (cl-destructuring-bind (r g b) (day2-to-rgb one-pull)
	(setf red (max red r) blue (max blue b) green (max green g))))
    (list red green blue)))

(defun day2-part2 (input-buffer)
  (let ((solution 0))
    (dolist (line (aoc-buffer-lines input-buffer) solution)
      (let ((game (string-split line "[:;]")))
	(cl-destructuring-bind (r g b) (day2-fewest (cdr game))
	  (cl-incf solution (* r g b)))))))

;; (aoc-copy-output () (day2-part2 "day2.2023.input.txt"))
;; (aoc-copy-output () (day2-part2 "test.buff"))
