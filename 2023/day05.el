;;; Day 5: If You Give A Seed A Fertilizer
;;; Advent of Code 2023
;;; Tuesday, December 05, 2023

;;       --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;;   5   00:44:04   5483      0   22:10:50  40131      0

;; hacky part 2 solution takes 82 seconds to run

(require 'cl-lib)

(defun day5-parse-input (buffer-name)
  (let ((solution 0)
	(seeds ())
	(map-name "")
	(map-order ())
	(maps (make-hash-table :test 'equal)))
    (dolist (line (aoc-buffer-lines buffer-name))
      (cond ((string-match "^seeds:" line)
	     (setq seeds (mapcar #'string-to-number (cdr (string-split line)))))
	    ((string-equal "" line))
	    ((string-match "map:" line)
	     (setq map-name (car (string-split line)))
	     (push map-name map-order)
	     (setf (gethash map-name maps) ()))
	    (t (let ((map (mapcar #'string-to-number (string-split line))))
		 (push map (gethash map-name maps))))))
    (setq map-order (reverse map-order))
    (list seeds map-order maps)))

(defun day5-part-1 (buffer-name)
  (cl-destructuring-bind (seeds map-order maps) (day5-parse-input buffer-name)
    (apply #'min (mapcar (lambda (seed)
			   (dolist (name map-order seed)
			     (setq seed (day5-map-one seed (gethash name maps)))))
			 seeds))))

(defun day5-map-one (seed map)
  (let ((answer seed))
    (dolist (triple map answer)
      (cl-destructuring-bind (dst src range) triple
	  (when (<= src seed (+ src (1- range)))
	    (setq answer (+ dst (- seed src))))))))

;; (aoc-copy-output () (day5-part-1 "day5.2023.input.txt"))
;;  (day5-part-1 "test.buff")

(defun day5-part-2 (buffer-name)
  (cl-destructuring-bind (seeds map-order maps) (day5-parse-input buffer-name)
    (let ((min (+ 4294438171 529125))
	  (count 0)
	  (start nil))
      ;; find a low number without trying every seed
      (mapc (lambda (pair)
	      ;;(message (format "pair: %d, min: %d" (cl-incf count) min))
	      (cl-do ((seed (car pair) (+ 32211 seed)))
		  ((>= seed (+ (car pair) (cdr pair))) min)
		(let* ((n seed)
		       (v (dolist (name map-order n)
			    (setq n (day5-map-one n (gethash name maps))))))
		  (when (< v min)
		    (setf min v start seed)))))
	    (day5-seed-pairs seeds))
      ;; work backwards from the low number to find a lower one
      (cl-do ((seed start (1- seed))
	      (j 32211 (1- j)))
	  ((or (zerop seed) (zerop j)))
	(let* ((n seed)
	       (v (dolist (name map-order n)
		    (setq n (day5-map-one n (gethash name maps))))))
	  (when (< v min)
	    (setf min v start seed))))
      min)))

;; 78775051 -- woot!
;; 78972903 -- too high

;; (benchmark-run (aoc-copy-output () (day5-part-2 "day5.2023.input.txt")))
;; (day5-part-2 "test.buff")

(defun day5-seed-pairs (seeds)
  (if (null seeds)
      ()
    (cons (cons (car seeds) (cadr seeds)) (day5-seed-pairs (cddr seeds)))))
 
