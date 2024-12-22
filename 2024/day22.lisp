;;; Advent of Code
;;; Sunday, December 22, 2024

;;       --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;;  22   00:48:29   4273      0   12:54:33  12709      0

(defun prune (n)
  (mod n 16777216))

(defun next-secret (s)
  (let* ((u (prune (logxor s (* s 64))))
	 (v (prune (logxor u (floor (/ u 32))))))
      (prune (logxor v (* v 2048)))))

(defun gen-numbers (seed times)
  (do ((k 0 (1+ k))
       (n seed (next-secret n)))
      ((= k times) n)))

(defun day22-part1 (file)
  (with-open-file (s file)
    (let ((sum 0))
      (do ((n #1=(read s nil nil) #1#))
	  ((null n) sum)
	(incf sum (gen-numbers n 2000))))))

;;(day22-part1 "input.22")

;; part 2...

(defun gen-diffs (seed times)
  (let ((diffs ())
	(nums ())
	(prev (mod seed 10)))
    (do ((k 0 (1+ k))
	 (n (next-secret seed) (next-secret n)))
	((= k times) (list (reverse diffs) (reverse nums)))
      (push (mod n 10) nums)
      (push (- (mod n 10) prev) diffs)
      (setq prev (mod n 10)))))
;; (gen-diffs 123 10)

(defun first-four (list)
  (if (null (fourth list))
      nil
      (list (first list) (second list) (third list) (fourth list))))

(defun update-first-keys (diffs nums sums seen)
  (when (fourth diffs)
    (let ((key (first-four diffs)))
      (unless (gethash key seen)
	(setf (gethash key seen) t)
	(incf (gethash key sums 0) (fourth nums)))
      (update-first-keys (cdr diffs) (cdr nums) sums seen))))

(defun day22-search (data)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (d data)
      (update-first-keys (car d) (cadr d) ht (make-hash-table :test 'equal)))
    (let ((high 0))
      (maphash #'(lambda (k v) (setq high (max high v))) ht)
      high)))

(defun day22-part2 (file)
  (with-open-file (in file)
    (do ((n #1=(read in nil nil) #1#)
	 (diffs ()))
	((null n) (reverse diffs))
      (push (gen-diffs n 2000) diffs))))

(defun day22-part2-solve (file)
  (let ((data (day22-part2 file)))
    (day22-search data)))

;; (time (day22-part2-solve "input.22"))
;; => 2.463861 seconds of total run time (2.246789 user, 0.217072 system)

