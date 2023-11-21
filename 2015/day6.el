;;; day7.el - advent of code 2015
;;; Saturday, November 18, 2023
;;; warming up for 2023

;;(aoc-make-grid 2 2)

(defun make-point (s)
  (mapcar #'string-to-number (string-split s ",")))
;;(make-point "44,66")

;; (defun toggle (grid x y)
;;   (let ((v (aoc-grid-get grid x y)))
;;     (aoc-grid-set grid x y (abs (1- v)))))

;; (defun turn-on (grid x y)
;;   (aoc-grid-set grid x y 1))

;; (defun turn-off (grid x y)
;;   (aoc-grid-set grid x y 0))

(defun toggle (grid x y)
  (let ((v (aoc-grid-get grid x y)))
    (aoc-grid-set grid x y (+ 2 v))))

(defun turn-on (grid x y)
  (let ((v (aoc-grid-get grid x y)))
    (aoc-grid-set grid x y (1+ v))))

(defun turn-off (grid x y)
  (let ((v (aoc-grid-get grid x y)))
    (aoc-grid-set grid x y (max 0 (1- v)))))

(defun do-lights (line grid)
  (let ((s (string-replace "turn o" "turn-o" line)))
    (let ((toks (string-split s)))
      (cl-destructuring-bind (cmd start through end) toks
	(let ((start (make-point start))
	      (end (make-point end))
	      (fun (cond ((equal "toggle" cmd) #'toggle)
			 ((equal "turn-on" cmd) #'turn-on)
			 ((equal "turn-off" cmd) #'turn-off))))
	  (aoc-walk-grid grid fun start end))))))
	   
(aoc-copy-output nil
 (let ((grid (aoc-make-grid 1000 1000)))
   (dolist (line (aoc-buffer-lines "day6.input.txt"))
     (message line)
     (do-lights line grid))
   (let ((count 0))
     (aoc-walk-grid
      grid
      (lambda (g x y) (cl-incf count (aoc-grid-get g x y))))
     count)))
