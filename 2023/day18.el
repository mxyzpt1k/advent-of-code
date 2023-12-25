;;; Day 18: Lavaduct Lagoon
;;; Advent of Code 2023
;;; Monday, December 18, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  18   01:16:41   4047      0   19:04:58  14661      0

(defun day18-part-1 (buffer-name)
  (let ((max-lisp-eval-depth 5000)
	(plan ())
	(*path* (list (cons 0 0))))
    (dolist (line (aoc-buffer-lines buffer-name))
      (apply 'day18-dig-part-1 (string-split line)))
    (let ((x-min (caar *path*))
	  (x-max (caar *path*))
	  (y-max (cdar *path*))
	  (y-min (cdar *path*)))
      (dolist (p *path*)
	(setq x-max (max x-max (car p)))
	(setq y-max (max y-max (cdr p)))
	(setq x-min (min x-min (car p)))
	(setq y-min (min y-min (cdr p))))
      (setq *path* (mapcar (lambda (p)
			     (cons (+ (car p) (abs x-min)) (+ (cdr p) (abs y-min))))
			   *path*))
      (setq y-max (+ (abs y-min) y-max))
      (setq x-max (+ (abs x-min) x-max))
      (setq y-min (+ (abs y-min) y-min))
      (setq x-min (+ (abs x-min) x-min))
      (let ((*grid* (make-vector (1+ y-max) nil)))
	(dotimes (row (1+ y-max))
	  (setf (aref *grid* row) (make-vector (1+ x-max) ?.)))
	(day18-plot (car *path*) (cdr *path*))
	(let ((*grid* (aoc-add-border (aoc-add-border *grid* ?.) ?@)))
	  (day18-fill-grid 1 1)
	  (day17-display *grid* ())
	  (let ((count 0))
	    (dotimes (row (length *grid*) count)
	      (dotimes (col (length (aref *grid* 0)))
		(when (or (= ?. (day10-get *grid* row col))
			  (= ?# (day10-get *grid* row col)))
		  (cl-incf count))))))))))
      
(defun day18-dig-part-1 (dir num color)
  (let ((n (string-to-number num))
	(x (caar *path*))
	(y (cdar *path*)))
    (cond ((equal "D" dir) (push (cons x (- y n)) *path*))
	  ((equal "U" dir) (push (cons x (+ y n)) *path*))
	  ((equal "R" dir) (push (cons (+ x n) y) *path*))
	  ((equal "L" dir) (push (cons (- x n) y) *path*))
	  (t (error "unexpectd direction: %s" dir)))))

(defun day18-plot (pt path)
  (if (null path)
      t
    (day18-draw-line pt (car path))
    (day18-plot (car path) (cdr path))))

(defun day18-draw-line (a b)
  (if (equal a b)
      t
    (day10-set *grid* (cdr a) (car a) ?#)
    (day18-draw-line (day18-step a b) b)))

(defun day18-step (a b)
  (if (= (car a) (car b))
      (if (> (cdr a) (cdr b))
	  (cons (car a) (1- (cdr a)))
	(cons (car a) (1+ (cdr a))))
    (if (> (car a) (car b))
	(cons (1- (car a)) (cdr a))
      (cons (1+ (car a)) (cdr a)))))

(defun day18-fill-grid (row col)
  (let ((work (list (cons row col))))
    (while work
      (let* ((p (pop work))
	     (row (car p))
	     (col (cdr p)))
	(when (= ?. (day10-get *grid* row col))
	  (day10-set *grid* row col ?%)
	  (push (cons (1+ row) col) work)
	  (push (cons (1- row) col) work)
	  (push (cons row (1- col)) work)
	  (push (cons row (1+ col)) work))))))
  
;; (day18-part-1 "test.buff")
;; (aoc-copy-output () (day18-part-1 "day18.2023.input.txt") )


;;; part 2

(cl-defstruct cpoint x y dir)

(defun day18-dig-part-2 (dir num color)
  (let ((num (string-to-number (substring color 2 (+ -2 (length color))) 16))
	(dir (substring color (+ -2 (length color)) (1- (length color))))
	(x (cpoint-x (car *path*)))
	(y (cpoint-y (car *path*))))
    (cond ((equal "0" dir) (push (make-cpoint :x (+ num x) :y y :dir 'right) *path*)) ;R
	  ((equal "1" dir) (push (make-cpoint :x x :y (- y num) :dir 'down) *path*)) ;D
	  ((equal "2" dir) (push (make-cpoint :x (- x num) :y y :dir 'left) *path*)) ;L
	  ((equal "3" dir) (push (make-cpoint :x x :y (+ num y) :dir 'up) *path*)) ;U
	  (t (error "unexpectd direction: %s" dir)))))

;; checking 
;(day18-dig-part2 1 1 "(#d2c081)") ; 863240
;(format "%x" 863240)

(defun day18-part-2 (buffer-name)
  (let ((*path* (list (make-cpoint :x 0 :y 0 :dir 'start)))
	(*area* 0))
    (dolist (line (aoc-buffer-lines buffer-name))
      (apply 'day18-dig-part-2 (string-split line)))
    ;; output a list of points to feed into mathematica
    ;; too bad i didn't thinkg of this for the first half
    (string-join (mapcar (lambda (p) (format "{%d,%d}" (cpoint-x p) (cpoint-y p))) *path*) ",")))

;; (aoc-copy-output () (day18-part-2 "day18.2023.input.txt"))
