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


;;; part 2

(defun day12-gen-dots-between (pattern size chunks)
  (let ((limit (length pattern)))
    
  )

(defun day12-gen-dots (pattern size chunks)
  (let ((sum (seq-reduce '+ chunks 0)))
    (let ((rem (- size sum))
	  (start (aref pattern 0))
	  (end (arf pattern (1- (length pattern)))))
      (cond ((= ?# start end) (day12-gen-dots-between pattern rem chunks))
	    ((= ?# start) (day12-gen-end-dot pattern rem chunks))
	    ((= ?# end) (day12-gen-start-dot pattern rem chunks))
	    (t (day12-gen-both-dots pattern rem chunks))))))

(defun day12-gen-possibilities-2 (springs sizes attempt start-pos)
  (cond ((null sizes) (if (day12-match springs attempt) 1 0))
	((> (+ (1- (length sizes) (cl-sum sizes))) (length springs)) 0)
	((= ?# (aref attempt start-pos))
	 
	 (day12-gen-possibilities-2 springs sizes (s
	(t (let* ((chunk (make-string (car sizes) ?#))
		  (next (day12-splice attempt chunk start-pos)))
	     (+ (day12-gen-possibilities-2 springs (cdr sizes) next (+ start-pos (car sizes) 1))
		(day12-gen-possibilities-2 springs sizes attempt (+ 1 start-pos)))
      
  (let ((count 0))
    (while sizes
      (let ((chunk (make-string (pop sizes) ?#)))
	(dotimes (k (- (length springs) start-pos))
	  (let ((s (day12-splice 
	
  (if (null sizes)
      count
    (dotimes (k (length springs))
      
  (let ((chunks (mapcar (lambda (n) (make-string n ?#)) sizes)))
    chunks))

(defun day12-unfold (str sep)
  (let ((acc ()))
    (dotimes (k 5 (string-join acc sep))
      (push str acc))))

(defun day12-part-2 (buffer-name)
  (let ((total 0)
	(progress 0))
    (dolist (line (aoc-buffer-lines buffer-name) total)
      (message (format "progress: %d" (cl-incf progress)))
      (seq-let (springs sizes) (string-split line)
	(let ((springs (day12-unfold springs "?")) ;not gonna work
	      (sizes (day12-unfold sizes ",")))
	  (setq sizes (mapcar 'string-to-number (string-split sizes ",")))
	  (let ((n (day12-gen-possibilities springs sizes)))
	    (cl-incf total n))
	  )))))
