;;; Day 13: Point of Incidence
;;; Advent of Code 2023
;;; Wednesday, December 13, 2023

(defun column-equal-p (map a b)
  (let ((answer t))
    (dotimes (r (length map) answer)
      (let ((row (aref map r)))
	(unless (equal (aref row a) (aref row b))
	  (setq answer nil))))))

(defun row-equal-p (map a b)
  (equal (aref map a) (aref map b)))

(defun mirror-size (vec a b upper-limit predicate)
  (cond ((or (< a 0) (>= b upper-limit))
	 (setq *goes-to-edge* t)
	 0)
	((funcall predicate vec a b)
	 ;;(message (format "%d = %d" a b))
	 (1+ (mirror-size vec (1- a) (1+ b) upper-limit predicate)))
	(t 0)))

(defun find-mirrors (vec)
  (let ((lines 0)
	(*goes-to-edge* nil)
	(rows (length vec))
	(cols (length (aref vec 0))))
    (dotimes (k (1- cols))
      (when (column-equal-p vec k (1+ k))
	(let ((s (mirror-size vec k (1+ k) cols 'column-equal-p)))
	  (when *goes-to-edge*
	    (setq lines (+ k 1))
	    (part2-reflection 'column (1+ k))
	    (setq *goes-to-edge* nil)))))
    (dotimes (k (1- rows))
      (when (row-equal-p vec k (1+ k))
	(let ((s (mirror-size vec k (1+ k) rows 'row-equal-p)))
	  (when *goes-to-edge*
	    (setq lines (* 100 (+ k 1)))
	    (part2-reflection 'row (1+ k))
	    (setq *goes-to-edge* nil)))))
    lines))

(defun list-to-mirror (list)
  (seq-into (reverse list) 'vector))

(defun day13-part-1 (buffer-name)
  (let ((acc ())
	(score 0))
    (dolist (line (aoc-buffer-lines buffer-name))
      (cond ((string= "" line)
	     (cl-incf score (find-mirrors (list-to-mirror acc)))
	     (setq acc ()))
	    (t (push line acc))))
    (+ score (find-mirrors (list-to-mirror acc)))))

;; (aoc-copy-output () (day13-part-1 "day13.2023.input.txt"))
;; (day13-part-1 "test.buff")


