;;;
;;; Advent of Code 2023
;;;

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
    (let ((min nil))
      (mapc (lambda (pair)
	      (cl-do ((seed (car pair) (1+ seed)))
		  ((= seed (+ (car pair) (cdr pair))) min)
		(dolist (name map-order seed)
		  (let ((v (day5-map-one seed (gethash name maps))))
		    (setq min (if (null min) v (min v min)))))))
	    (day5-seed-pairs seeds))
      min)))

(defun day5-seed-pairs (seeds)
  (if (null seeds)
      ()
    (cons (cons (car seeds) (cadr seeds)) (day5-seed-pairs (cddr seeds)))))
 

(cl-defstruct day5-range first size)

(cl-defmethod day5-range-last ((r day5-range))
  "return the last number in range"
  (+ (day5-range-first r) (+ -1 (day5-range-size r))))

(cl-defmethod day5-range-member ((r day5-range) (seed number))
  "return true if range a contains the seed number"
  (<= (day5-range-first r) seed (day5-range-last r)))

(cl-defmethod day5-range-slide-right ((dst day5-range) (src day5-range) (seed number))
  (let* ((src-a (day5-range-first src))
	 (src-z (day5-range-last src))
	 (src-n (day5-range-size src))
	 (dst-a (day5-range-first dst))
	 (dst-z (day5-range-last dst))
	 (dst-n  (day5-range-size dst))
	 (offset (- src-z seed))
	 (start (+ offset dst-a))
	 (end+1 (- src-n dst-n))
	 (size (if (< end+1 dst-z)
		   0
		 (1+ (- dst-z (+ offset dst-a))))))))
    (make-day5-range :first (day5-range-last  :size size)))

(cl-defmethod day5-range-slide-left ((dst day5-range) (src day5-range) (seed number))
  ;; aaaaaaaa.aaa            [a   12]
  ;; bbbbbbbb[.bbb]          [b+8  4]
  ;;             cccccccc    [a+13 8]
  (let* ((src-a (day5-range-first src))
	 (src-z (day5-range-last src))
	 (src-n (day5-range-size src))
	 (dst-a (day5-range-first dst))
	 (dst-z (day5-range-last dst))
	 (dst-n  (day5-range-size dst))
	 (offset (- seed src-a))
	 (start (+ offset dst-a))
	 (end+1 (+ start src-n))
	 (size (if (< end+1 dst-z)
		   src-n
		 (1+ (- dst-z start)))))
    (make-day5-range :first start :size size)))

(let ((src (make-day5-range :first 10 :size 50))
      (dst (make-day5-range :first 20 :size 10)))
  (day5-range-slide-left dst src 12))

(cl-defmethod day5-range-split ((dst day5-range) (src day5-range) (seed number))
  (if (not (day5-range-member src seed))
      ()
    (let ((src-a (day5-range-first src))
	  (src-z (day5-range-last src))
	  (src-n (day5-range-size src))
	  (dst-a a (day5-range-first dst))
	  (dst-z (day5-range-last dst))
	  (dst-n  (day5-range-size dst)))
      (let* ((offset (- seed src-a))
	     (new-start (+ offset dst-a))
	     (new-size (if (< (+ new-start src-n) dst-z)
			   (+ new-start src-n)
			 (
	     
	   (dst-size (if (< (day5-range-end src) (+ seed (day5-range-size src)))
			 
	   (dst-end (
	   (dst-size (min (+ 1 (- (day5-range-last dst) (day5-range-first dst)))
			  (+ 1 (- (day5-range-first dst) offset)
	   (rem-start (+ seed dst-size))
	   (rem-size (- (day5-range-size src) dst-size)))
      (if (< 0 rem-size)
	  (list #1=(make-day5-range :first dst-start :size dst-size))
	(list #1# (make-day5-range :first rem-start :size rem-size))))))


(let ((src (make-day5-range :first 1 :size 4))
      (dst (make-day5-range :first 51 :size 40))
      (seed 4))
  (day5-range-split dst src 4))



