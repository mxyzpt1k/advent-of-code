;;; Advent of Code 2024
;;; Sunday, December 22, 2024

;; day 12, part 2

(defvar data)				;2x2 array

(defun walk-grid (fun data)
  (destructuring-bind (rows cols) (array-dimensions data)
    (dotimes (r rows)
      (dotimes (c cols)
	(funcall fun data r c)))))

(defun grid-ref (grid row col)
  (destructuring-bind (rs cs) (array-dimensions grid)
    (cond ((or (< row 0) (< col 0)) nil)
	  ((or (<= rs row) (<= cs col)) nil)
	  (t (aref grid row col)))))

(defun get-region (data row col)
  (let ((queue (list (list row col)))
	(this (aref data row col))
	(seen (make-hash-table :test 'equal)))
    (setf (gethash (list row col) seen) t) ; <-- !! for blocks of one block
    (flet ((test-push (r c)
	     (when (eql this (grid-ref data r c))
	       (let ((point (list r c)))
		 (unless (gethash point seen nil)
		   (setf (gethash point seen) t)
		   (push point queue))))))
      (loop while queue
	    do (let ((p (pop queue)))
		 (let ((r (first p))
		       (c (second p)))
		   (test-push (1- r) c)
		   (test-push (1+ r) c)
		   (test-push r (1- c))
		   (test-push r (1+ c)))))
      seen)))

(defun top-edges (offset points)
  (cond ((null points) ())
	((null (cdr points)) points)
	((and (= (first (car points)) (first (cadr points)))
	      (= (+ offset (second (car points))) (second (cadr points))))
	 (top-edges offset (cdr points)))
	(t (cons (car points) (top-edges offset (cdr points))))))

(defun left-edges (offset points)
  (cond ((null points) ())
	((null (cdr points)) points)
	((and (= (second (car points)) (second (cadr points)))
	      (= (+ offset (first (car points))) (first (cadr points))))
	 (left-edges offset (cdr points)))
	(t (cons (car points) (left-edges offset (cdr points))))))

(defun top-sort (a b)
  (cond ((< (car a) (car b)) t)
	((= (car a) (car b)) (< (cadr a) (cadr b)))
	(t nil)))

(defun left-sort (a b)
   (cond ((< (cadr a) (cadr b)) t)
	((= (cadr a) (cadr b)) (< (car a) (car b)))
	(t nil)))

(defun get-edges (data region)
  (let ((tops ())
	(bottoms ())
	(rights ())
	(lefts()))
    (maphash #'(lambda (point b)
		 (declare (ignore b))
		 (destructuring-bind (r c) point
		     (let ((this (grid-ref data r c)))
		       (unless (eql this (grid-ref data (1- r) c))
			 (push point tops))
		       (unless (eql this (grid-ref data (1+ r) c))
			 (push point bottoms))
		       (unless (eql this (grid-ref data r (1+ c)))
			 (push point rights))
		       (unless (eql this (grid-ref data r (1- c)))
			 (push point lefts)))))
	     region)
    (+ (length (top-edges 1 (sort tops #'top-sort)))
       (length (top-edges -1 (reverse (sort bottoms #'top-sort))))
       (length (left-edges -1 (reverse (sort rights #'left-sort))))
       (length (left-edges 1 (sort lefts #'left-sort))))))

(defun mark-region (data region &optional (mark :done))
  (maphash #'(lambda (point b)
	       (declare (ignore b))
	       (destructuring-bind (r c) point
		 (setf (aref data r c) mark)))
	   region))

(defun day12-read (input)
  (flet ((to-array (ll)
	   (make-array `(,(length ll) ,(length (car ll)))
		       :initial-contents ll)))
    (with-open-file (s input)
      (do ((line #1=(read-line s nil nil) #1#)
	   (acc ()))
	  ((null line) (to-array (reverse acc)))
	(push (map 'list #'identity line) acc)))))

(let ((data (day12-read "input.12"))
      (sum 0))
  (walk-grid #'(lambda (g r c)
		 (when (not (eq :done (aref g r c)))
		   (let* ((region (get-region data r c))
			  (edges (get-edges data region)))
		     ;;(print (list (grid-ref g r c) edges region))
		     (incf sum (* edges (hash-table-count region)))
		     (mark-region data region :done))))
	     data)
  sum)

;; 919780 is too high
;; 854282 is too low
