;;; Day 12: Hot Springs
;;; Advent of Code 2023
;;; Tuesday, December 12, 2023

(defun day12-match (pattern string)
  (let ((match t))
    (dotimes (k (length pattern) match)
      (cond ((= (aref pattern k) (aref string k)) t)
	    ((= (aref pattern k) ??) t)
	    (t (setq match nil))))))

(defun day12-size-check (pattern sizes)
  (equal sizes (mapcar 'length (string-split pattern "\\." t))))

(defun day12-bit-set (pattern unknowns)
  (cond ((null unknowns) pattern)
	((= ?# (aref pattern (car unknowns)))
	 (setf (aref pattern (car unknowns)) ?.)
	 (day12-bit-set pattern (cdr unknowns)))
	(t (setf (aref pattern (car unknowns)) ?#))))
		    
(defun day12-gen-possibilities (springs sizes)
  (let ((unknowns ())
	(attempt (seq-into springs 'string))
	(count 0))
    (dotimes (k (length springs))
      (when (= ?? (aref springs k))
	(setf (aref attempt k) ?.)
	(push k unknowns)))
    (dotimes (k (expt 2 (length unknowns)))
      (day12-bit-set attempt unknowns)
      (when (and (day12-size-check attempt sizes)
		 (day12-match springs attempt))
	(cl-incf count)))
    count))

(defun day12-part-1 (buffer-name)
  ;; not fast but it got me there. won't work for part 2
  (let ((total 0)
	(progress 0))
    (dolist (line (aoc-buffer-lines buffer-name) total)
      (message (format "progress: %d" (cl-incf progress)))
      (seq-let (springs sizes) (string-split line)
	(setq sizes (mapcar 'string-to-number (string-split sizes ",")))
	(let ((n (day12-gen-possibilities springs sizes)))
	  (cl-incf total n))))))

;; (day12-part-1 "day12.2023.input.txt")

