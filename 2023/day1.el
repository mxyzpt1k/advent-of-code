;;; Day 1: Trebuchet?!
;;; Advent of Code 2023
;;; Friday, December 01, 2023

;;; Day       Time  Rank  Score       Time  Rank  Score
;;;   1   00:11:52  5738      0   00:44:31  5100      0

;;; It took a while to find a missing a pipe in the regexp in part 2.

;;; part 1

(aoc-copy-output ()
  (let ((sum 0))
    (dolist (line (aoc-buffer-lines "day1.2023.input.txt") sum)
      (let ((got-one nil)
	    (first ?0)
	    (last ?0))
	(dolist (c (string-to-list line))
	  (when (cl-digit-char-p c)
	    (if got-one
		(setf last c)
	      (setf first c last c got-one t))))
	(let ((number (string-to-number (format "%c%c" first last))))
	  (cl-incf sum number))))))

;;; part 2

(defun day1-digit (line rgx)
  (let ((lookup `(("one" . "1")
		  ("two" . "2")
		  ("three" . "3")
		  ("four" . "4")
		  ("five" . "5")
		  ("six" . "6")
		  ("seven" . "7")
		  ("eight" . "8")
		  ("nine" . "9")
		  ("zero" . "0"))))
    (cl-assert (string-match rgx line))
    (let ((match (match-string 1 line)))
      (or (cdr (assoc match lookup))
	  match))))

(defun day1-first-digit (line)
  (day1-digit
   line
   "\\(zero\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[0-9]\\)"))

(defun day1-last-digit (line)
  (day1-digit
   line
   ".*\\(zero\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[0-9]\\)"))

(defun day1-part2 (input-buffer)
  (aoc-copy-output ()
    (let ((sum 0))
      (dolist (line (aoc-buffer-lines input-buffer) sum)
	(let ((first (day1-first-digit line))
	      (last (day1-last-digit line)))
	  (let ((number (string-to-number (format "%s%s" first last))))
	    (cl-assert (< 0 number 100))
	    (cl-incf sum number)))))))

;; (day1-part2 "test.buff")
;; (day1-part2 "day1.2023.input.txt")

