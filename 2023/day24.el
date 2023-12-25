;;; Day 24: Never Tell Me The Odds 
;;; Advent of Code 2023
;;; Sunday, December 24, 2023

(require 'peg)

(defconst hail-min-position 200000000000000)

(defconst hail-max-position 400000000000000)

(defun hail-zerop (f)
  (< (abs f) 1e-8))

(defun hail-past-position-p (x0 y0 x1 y1 dx dy)
  (not (and (< 0 (* dx (- x1 x0)))
	    (< 0 (* dy (- y1 y0))))))

(defun hail-intersect-p (a b)
  (seq-let ([x1 y1 z1] [dx1 dy1 dz1]) a
    (seq-let ([x2 y2 z2] [dx2 dy2 dz2]) b
      (let ((m1 (/ dy1 (+ 0.0 dx1)))
	    (m2 (/ dy2 (+ 0.0 dx2))))
	(let ((b1 (- y1 (* m1 x1)))
	      (b2 (- y2 (* m2 x2))))
	  (if (hail-zerop (- m1 m2))
	      nil
	    (let* ((x (/ (- b2 b1) (- m1 m2)))
		   (y (+ b1 (* m1 x))))
	      (cond ((hail-past-position-p x1 y1 x y dx1 dy1) nil)
		    ((hail-past-position-p x2 y2 x y dx2 dy2) nil)
		    (t (and (<= hail-min-position x hail-max-position)
			    (<= hail-min-position y hail-max-position)))))))))))

(defun day24-check-paths (hail count)
  (cond ((null hail) count)
	(t (dolist (h (cdr hail))
	     (when (hail-intersect-p (car hail) h)
	       (cl-incf count)))
	   (day24-check-paths (cdr hail) count))))

(defun day24-part1 (buffer-name)
  (let ((lines (day24-read-input buffer-name))
	(count 0))
    (day24-check-paths lines count)))

(let ((hail-max-position 27)
      (hail-min-position 7))
  (day24-part1 "test.buff.24"))


;; (aoc-copy-output () (day24-part1 "day24.2023.input.txt"))

(defun day24-parse-hail ()
  (with-peg-rules
      ((hail    (bol) (list triple) " @" (+ " ") (list triple) (eol)
		`(a b -- (vconcat b) (vconcat a)))
       (triple  number "," (+ " ") number "," (+ " ") number)
       (sign    (or "+" "-" ""))
       (digit   [0-9])
       (number  (substring sign (+ digit))
		`(a -- (string-to-number a))))
    (peg-run (peg hail))))

(defun day24-read-input (buffer-name)
  (with-current-buffer buffer-name
    (goto-char (point-min))
    (let ((data ())
	  (hail (day24-parse-hail)))
      (while hail
	(push hail data)
	(forward-line)
	(setq hail (day24-parse-hail)))
      (reverse data))))

