;;; Day 7: Camel Cards
;;; Advent of Code 2023
;;; Thursday, December 07, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   7   21:37:15  54427      0   22:51:40  49034      0

(require 'cl-lib)

(cl-defstruct day7-hand cards bid value)

(defun day7-part-1 (buffer-name)
  (let ((hands (mapcar #'day7-split (aoc-buffer-lines buffer-name)))
	(rank 0))
    (seq-reduce (lambda (acc hand)
		  (+ acc (* (day7-hand-bid hand) (cl-incf rank))))
		(sort hands #'day7-hand-less-p)
		0)))

(defun day7-split (line)
  (let ((pair (string-split line)))
    (let ((card-values (apply 'vector (mapcar #'day7-card-value (car pair)))))
      (make-day7-hand :value (day7-score-hand card-values)
		      :cards card-values
		      :bid (string-to-number (cadr pair))))))

(defun day7-card-value (card)
  (cl-case card
    (?A 14)
    (?K 13)
    (?Q 12)
    (?J 11)
    (?T 10)
    (otherwise (- card ?0))))

(defun day7-score-hand (hand)
  (let ((v (make-vector 15 0)))
    (seq-do (lambda (n) (cl-incf (aref v n))) hand)
    (cond ((seq-contains-p v 5) 6)
	  ((seq-contains-p v 4) 5)
	  ((and (seq-contains-p v 3) (seq-contains-p v 2)) 4)
	  ((seq-contains-p v 3) 3)
	  ((seq-contains-p v 2) (cl-count 2 v))
	  (t 0))))

(defun day7-hand-less-p (a b)
  (cond ((< (day7-hand-value a) (day7-hand-value b)) t)
	((< (day7-hand-value b) (day7-hand-value a)) nil)
	(t (let ((A (day7-hand-cards a))
		 (B (day7-hand-cards b)))
	     (cl-dotimes (i 5)
	       (cond ((< (aref A i) (aref B i)) (cl-return t))
		     ((< (aref B i) (aref A i)) (cl-return nil))))))))

;; part 2 functions

(defun day7-card-value (card)
  (cl-case card
    (?A 14)
    (?K 13)
    (?Q 12)
    (?J 1)				;joker
    (?T 10)
    (otherwise (- card ?0))))

(defun day7-score-hand (hand)
  (let ((v (make-vector 15 0)))
    (seq-do (lambda (n) (cl-incf (aref v n))) hand)
    (let ((jokers (aref v 1)))
      (cond ((seq-contains-p v 5) 6)
	    ((seq-contains-p v 4) (if (zerop jokers) 5 6))
	    ((and (seq-contains-p v 3) (seq-contains-p v 2)) (if (zerop jokers) 4 6))
	    ((seq-contains-p v 3) (if (zerop jokers) 3 5))
	    ((and (seq-contains-p v 2) (= 2 (cl-count 2 v)))
	     (cond ((= 2 jokers) 5)
		   ((= 1 jokers) 4)
		   (t 2)))
	    ((seq-contains-p v 2) (if (zerop jokers) 1 3))
	    ((= 1 jokers) 1)
	    (t 0)))))

;; (aoc-copy-output () (day7-part-1 "test.buff"))
;; (aoc-copy-output () (day7-part-1 "day7.2023.input.txt"))

