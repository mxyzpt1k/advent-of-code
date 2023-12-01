;;; day18.el - advent of code 2015
;;; Thursday, November 23, 2023
;;; warming up for 2023


(defun day18-step (from to)
  (aoc-walk-grid
   from
   (lambda (g x y)
     (let ((n (count-neighbors g x y)))
       (if (or (= n 2) (= n 3))
	   (aoc-grid-set to x y 1)
	 (aoc-grid-set to x y 0))))))

(defun count-neighbors (g x y)
  (+ (aoc-grid-get-default g (1+ x) (1+ y) 0)
     (aoc-grid-get-default g (1+ x) y 0)
     (aoc-grid-get-default g (1+ x) (1- y) 0)
     (aoc-grid-get-default g x (1+ y) 0)
     (aoc-grid-get-default g x (1- y) 0)
     (aoc-grid-get-default g (1- x) (1+ y) 0)
     (aoc-grid-get-default g (1- x) y 0)
     (aoc-grid-get-default g (1- x) (1- y) 0)))

(defun day18 ()
  (day18-animate-grid (day18-read-grid) 100)
  (let ((count 0))
    (aoc-walk-grid
     (lambda (g x y) (cl-incf count (aoc-grid-get g x y)))
     count)))

(aoc-copy-output () (day18))

(defun day18-animate-grid (g times)
  (let ((g1 (make-aoc-grid :grid g
			   :x-size (length (aref g 0))
			   :y-size (length g)))
	(g2 (aoc-make-grid 100 100)))
    (dotimes (i (/ times 2)  g1)
      (day18-step g1 g2)
      (day18-step g2 g1))))

(defun day18-read-grid ()
  (let ((acc ())
	(lines (aoc-buffer-lines "day18.input.txt")))
    (dolist (line lines (seq-into (reverse acc) 'vector))
      (let ((row (seq-into (mapcar (lambda (c) (if (eql c ?.) 0 1)) line) 'vector)))
	(push row acc)))))
