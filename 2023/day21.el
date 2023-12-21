;;; Day 21: Step Counter
;;; Advent of Code 2023
;;; Thursday, December 21, 2023

(defun day21-part1 (buffer-name wanted-steps)
  (with-current-buffer buffer-name
    (let ((lines (mapcar 'vconcat (aoc-buffer-lines buffer-name))))
      (let ((grid (aoc-add-border (seq-into lines 'vector) ?%)))
	(let ((memo (make-hash-table :test 'equal))
	      (start (day21-start))
	      (count 0))
	  (day21-map-connected start 0)
	  (day21-count-steps)
	  ;;(message "%s" memo)
	  count)))))

(defun day21-map-connected (start distance)
  (setf (gethash start memo) distance)
  (dotimes (k (1+ wanted-steps))
    (maphash (lambda (pos dist)
	       (when (= k dist) (day21-map-helper pos (1+ dist))))
	     memo)))

(defun day21-map-helper (start distance)
  (dolist (dir '((-1 0) (1 0) (0 -1) (0 1)))
    (let ((pos (seq-mapn #'+ start dir)))
      (cond ((not (day21-garden-p pos)))
	    ((gethash pos memo))
	    (t (setf (gethash pos memo) distance))))))
	    
(defun day21-count-steps ()
  (day21-walk (lambda (row col chr)
		(let* ((pos (list row col))
		       (dist (gethash pos memo (1+ wanted-steps))))
		  (when (and (cl-evenp dist)
			     (<= dist wanted-steps))
		    (cl-incf count))))))

;; (day21-part1 "day21.2023.input.txt" 64)
;; (day21-part1 "test.buff" 6)

(defun day21-walk (func)
  (dotimes (row (length grid))
    (dotimes (col (length (aref grid 0)))
      (funcall func row col (aref (aref grid row) col)))))
  
(defun day21-start ()
  (catch 'start
    (day21-walk-grid (lambda (row col char)
		       (when (= ?S char)
			 (throw 'start (list row col)))))))

(defun day21-garden-p (pos)
  (seq-let (row col) pos
    (or (= ?. (aref (aref grid row) col))
	(= ?S (aref (aref grid row) col)))))
