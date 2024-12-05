;;; Day 23: A Long Walk
;;; Advent of Code 2023
;;; Saturday, December 23, 2023

(defun day23-part1 (buffer-name)
  (let ((grid (day23-read-input buffer-name))
	(memo (make-hash-table :test #'equal)))
    (day23-mark 1 2 0)
    ))

(defun day23-mark (row col steps)
  (setf (gethash (cons row col) memo) steps)
  (let ((c (day23-get row col)))
    (cond ((= c ?>) (day23-mark row (1+ col) (1+ steps)))
	  ((= c ?v) (day23-mark (1+ row) col (1+ steps)))
	  ((= c ?<) (day23-mark row (1- col) (1+ steps)))
	  ((= c ?^) (day23-mark (1- row) col (1+ steps)))
	  ((= c ?#) 'tree)
	  (t (dolist (dir '((-1 0) (0 1) (1 0) (0 -1)))
	       (day23-mark (+ row (car dir)) (+ col (cadr dir)) (1+ steps)))))))

(defun day23-get (row col)
  (aref (aref grid row) col))

(defun day23-read-input (buffer-name)
  (let ((lines (aoc-buffer-lines buffer-name)))
    (aoc-add-border (vconcat (mapcar #'vconcat lines)))))
