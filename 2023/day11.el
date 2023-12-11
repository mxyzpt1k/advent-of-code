;;; Day 11: Cosmic Expansion
;;; Advent of Code 2023
;;; Monday, December 11, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  11   00:33:08   4299      0   01:31:23   7253      0

(defun day11-expand (universe)
  ;; inserted new rows and returned a bigger universe for part 1.
  ;; returning a list of the empty rows for part 2.
  (let ((empty ())
	(idx 0))
    (seq-do (lambda (row)
	      (when (seq-every-p (lambda (c) (= c ?.)) row)
		(push idx empty))
	      (cl-incf idx))
	    universe)
    empty))

(defun day11-rotate (universe n)
  (if (zerop n)
      universe
    (let ((rows (length universe))
	  (cols (length (aref universe 0))))
      (let ((g2 ()))
	(dotimes (c cols (day11-rotate (vconcat g2) (1- n)))
	  (let ((v (make-vector rows 0)))
	    (dotimes (r rows)
	      (setf (aref v r) (aref (aref universe r) c)))
	    (push v g2)))))))
    
(defun day11-find-galaxies (universe)
  (let ((rows (length universe))
	(cols (length (aref universe 0)))
	(acc ()))
    (dotimes (r rows acc)
      (dotimes (c cols)
	(when (= ?# (aref (aref universe r) c))
	  (push (cons r c) acc))))))

(defun day11-distance (src dst)
  (+ (abs (- (car src) (car dst))) (abs (- (cdr src) (cdr dst)))))

(defun between? (a b c)
  (or (< a b c) (> a b c)))

(defun day11-distance-2 (src dst empty-rows empty-cols expansion)
  (let ((d (day11-distance src dst)))
    (dolist (r empty-rows)
      (when (between? (car src) r (car dst))
	(cl-incf d expansion)))
    (dolist (c empty-cols)
      (when (between? (cdr src) c (cdr dst))
	(cl-incf d expansion)))
    d))
  
(defun day11-solution (buffer-name expansion)
  (let ((universe (vconcat (mapcar 'vconcat (aoc-buffer-lines buffer-name)))))
    (let ((empty-rows (day11-expand universe))
	  (empty-cols (day11-expand (day11-rotate universe 3))))
      (let ((gs (day11-find-galaxies universe))
	    (sum 0))
	(while gs
	  (let ((src (pop gs)))
	    (dolist (dst gs)
	      (cl-incf sum (day11-distance-2 src dst empty-rows empty-cols expansion)))))
	sum))))

;; (day11-part-2 "test.buff" 1)

;; part 1
;; (aoc-copy-output () (day11-solution "day11.2023.input.txt" 1))

;; part 2
;; (aoc-copy-output () (day11-solution "day11.2023.input.txt" (1- 1000000)))
