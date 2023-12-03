;;; Day 3: Gear Ratios
;;; Avent of Code 2023
;;; Sunday, December 03, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   3   01:46:00  10922      0   02:26:16  10317      0

;;; part 1

(defun day3-part-numbers (buffer-name)
  (let ((input (apply #'vector (aoc-buffer-lines buffer-name))))
    (let ((grid (make-aoc-grid :x-size (length (aref input 0))
			       :y-size (length input) :grid input :x 0 :y 0)))
      (let ((*parts* ()))
	(aoc-walk-grid grid #'day3-neighbors)
	(aoc-sum (cl-remove-if #'null (day3-extract-numbers *parts*)))))))

;;; (aoc-copy-output () (day3-part-numbers "day3.2023.input.txt"))
;;; (aoc-copy-output () (day3-part-numbers "temp.buff"))

;;; part 2

(defun day3-gear-ratios (buffer-name)
  (let ((input (apply #'vector (aoc-buffer-lines buffer-name))))
    (let ((grid (make-aoc-grid :x-size (length (aref input 0))
			       :y-size (length input)
			       :grid input :x 0 :y 0)))
      (let ((sum 0))
	(aoc-walk-grid grid #'day3-get-gears)
	sum))))

(defun day3-get-gears (grid x y)
  (when (char-equal ?* (aoc-grid-get grid x y ?.))
    (message (format "gear at (%d %d)" x y))
    (let ((*parts* ()))
      ;;(grid (aoc-copy-grid g)))
      (day3-neighbors grid x y)
      (let ((nums (cl-remove-if #'null (day3-extract-numbers *parts*))))
	(when (= 2 (length nums))
	  (cl-incf sum (apply #'* nums)))))))

;;; (aoc-copy-output () (day3-gear-ratios "day3.2023.input.txt"))
;;; (aoc-copy-output () (day3-gear-ratios "temp.buff"))


;;; stuff for both parts -- mostly developed for part 1

(defun day3-symbol-p (c)
  (not (or (eql c ?.) (cl-digit-char-p c))))

(defun day3-extract-numbers (parts)
  ;; parts is a list of (x . y) pairs pointing to digits next to symbols
  ;; we need to turn them into numbers
  (let ((numbers ()))
    (dolist (p parts numbers)
      (push (day3-get-number-at p) numbers))))

(defun day3-get-number-at (p)
  (let ((x (car p))
	(y (cdr p)))
    (cond ((not (cl-digit-char-p (aoc-grid-get grid x y ?.))) nil)
	  ((cl-digit-char-p (aoc-grid-get grid (1- x) y ?.))
	   (day3-get-number-at (cons (1- x) y)))
	  (t ;; otherwise this is the start of a number
	   (let ((n 0))
	     (while (cl-digit-char-p (aoc-grid-get grid x y ?.))
	       (setf n (* 10 n))
	       (cl-incf n (- (aoc-grid-get grid x y) ?0))
	       (aoc-grid-set grid x y ?.)
	       (cl-incf x))
	     n)))))

(defun day3-digit-neighbor (grid x y)
  (when (cl-digit-char-p (aoc-grid-get grid x y ?.))
    (push (cons x y) *parts*)))

(defun day3-neighbors (grid x y)
  (when (day3-symbol-p (aoc-grid-get grid x y))
    (day3-digit-neighbor grid (1- x) (1- y))
    (day3-digit-neighbor grid (1- x) y)
    (day3-digit-neighbor grid (1- x) (1+ y))
    (day3-digit-neighbor grid x (1- y))
    (day3-digit-neighbor grid x (1+ y))
    (day3-digit-neighbor grid (1+ x) (1- y))
    (day3-digit-neighbor grid (1+ x) y)
    (day3-digit-neighbor grid (1+ x) (1+ y))))
