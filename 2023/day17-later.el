;;; Day 17: Clumsy Crucible
;;; Advent of Code 2023
;;; Saturday, December 16, 2023

(defun grid-ref (grid row col)
  (aref (aref grid row) col))

(defun grid-set (grid row col val)
  (let ((row (aref grid row)))
    (setf (aref row col) val)))

(defun day17-make-grid (buffer-name)
  (let ((acc ()))
    (dolist (line (aoc-buffer-lines buffer-name))
      (push (vconcat (seq-map (lambda (c) (- c ?0)) line)) acc))
    (vconcat (reverse acc))))

(defun day17-part1 (buffer-name)
  (let ((grid (day17-make-grid buffer-name))
	(memo (make-hash-table :test 'equal)))
    (day17-map-grid grid memo)))

(defun day17-map-grid (grid memo)
  (let ((path (list [0 0]))
	(score 0))
    (while path
      (let ((pos (pop path)))
	(seq-let [row col] pos
	  (let ((val (grid-ref grid row col)))
	    (if (< (+ score val) (gethash pos memo (+ 1 score val)))
		(setf (

	    ))))))
  
(day17-part1 "temp.buff")
    
		    
