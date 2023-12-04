;;; Day 4: Scratchcards
;;; Advent of Code 2023
;;; Monday, December 04, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   4   00:22:50   8350      0   01:21:03  11423      0

(require 'peg)

(defun day4-parse-card ()
  "parse the card input at point"
  (with-peg-rules
      ((card (bol) "Card" number ":" (list (+ number)) " |" (list (+ number)) (eol)
	     `(a b -- (list a b)))
       (number (+ space) (substring (+ digit))
	       `(n -- (string-to-number n)))
       (digit (range ?0 ?9))
       (space (syntax-class whitespace)))
    (reverse (peg-run (peg card)))))

(defun day4-part-1 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((card (day4-parse-card))
	  (solution 0))
      (while card
	(cl-destructuring-bind (n (winners numbers)) card
	  (let ((n (length (cl-intersection winners numbers))))
	    (when (> n 0)
	      (cl-incf solution (expt 2 (1- n))))))
	(forward-line)
	(setq card (day4-parse-card)))
      solution)))

;; (aoc-copy-output () (day4-part-1 "day4.2023.input.txt"))

(defun day4-part-2 (buffer-name)
  ;; read the cards into a list, then processing them in order
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((card `(0 ((0) (1)))) 	;start with a fake Card 0
	  (cards ()))
      (while card
	(push card cards)
	(setq card (day4-parse-card))
	(forward-line))
      (day4-count-copies (reverse cards)))))

(defun day4-count-copies (cards)
  (let ((copies (make-vector (length cards) 1)))
    (setf (aref copies 0) 0)		;the fake Card 0
    (seq-map-indexed (lambda (elt idx)
		       (cl-destructuring-bind (id (winners numbers)) elt
			 (let ((m (aref copies idx))
			       (n (length (cl-intersection winners numbers))))
			   (dotimes (k n)
			     (cl-incf (aref copies (+ idx k 1)) m)))))
		     cards)
    (seq-reduce #'+ copies 0)))

;; (aoc-copy-output () (day4-part-2 "day4.2023.input.txt"))
;; (aoc-copy-output () (day4-part-2 "test.buff"))
