;;; day17.el - advent of code 2015
;;; Thursday, November 23, 2023
;;; warming up for 2023

(defun day17 ()
  (let ((buckets (aoc-buffer-lines "day17.input.txt"))
	(solutions 0)
	(min-buckets 20))
    (solve17 150 (mapcar #'string-to-number buckets))
    solutions))

(defun marker-sum (bits size buckets)
  ;; day 17, part 2 was the first time i took advantage of dynamic scope
  (let ((sum 0)
	(len 0))
    (dotimes (i size sum)
      (when (bit-set-p bits i)
	(cl-incf len)
	(cl-incf sum (aref buckets i))))
    (when (= sum amount)
      (cond ((= len min-buckets) (cl-incf solutions))
	    ((< len min-buckets) (setf min-buckets len solutions 1))))
    sum))

(defun bit-set-p (num bit)
  (not (zerop (logand num (expt 2 bit)))))

(defun solve17 (amount buckets)
  (let ((bits 0)
	(size (length buckets))
	(buckets (seq-into buckets 'vector)))
    (dotimes (j (expt 2 size))
      (when (= amount (marker-sum j size buckets))
	(message (format "%d/%d" solutions j))))))

(aoc-copy-output () (day17))
