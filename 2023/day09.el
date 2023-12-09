;;; Day 9: Mirage Maintenance
;;; Advent of Code 2023
;;; Saturday, December 09, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   9   00:16:45   3098      0   00:17:44   2073      0

(defun day9 (buffer-name)
  (let ((solution 0))
    (dolist (line (aoc-buffer-lines buffer-name) solution)
      (let ((nums (mapcar 'string-to-number (string-split line))))
	;; part 1
	(cl-incf solution (extrapolate (reverse nums)))
	;; part 2
	(cl-incf solution (extrapolate nums))))))

(defun extrapolate (nums &optional acc)
  (if (cl-every 'zerop nums)
      (solve 0 acc)
    (extrapolate (next-level nums) (cons nums acc))))

(defun solve (n nums)
  (if (null nums)
      n
    (solve (+ n (caar nums)) (cdr nums))))

(defun next-level (nums)
  (cond ((null nums) ())
	((null (cdr nums)) ())
	(t (cons (- (car nums) (cadr nums)) (next-level (cdr nums))))))

;; (aoc-copy-output () (day9 "day9.2023.input.txt"))
