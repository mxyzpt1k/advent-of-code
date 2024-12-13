;;; Advent of Code
;;; Friday, December 13, 2024

;;       --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;;  13   01:04:01   6255      0   02:50:46   7167      0

(defun cost (ax ay bx by x0 y0)
  (let* ((ma (/ ay ax))
	 (mb (/ by bx))
	 (int (/ (+ (* (- mb) x0) y0) (- ma mb))))
    (if (and (integerp (/ int ax))
	     (integerp (/ (- x0 int) bx)))
	(+ (* 3 (/ int ax)) (/ (- x0 int) bx))
	0)))

(defun day13-part2 (file)
  (let ((cost 0))
    (with-open-file (s file)
      (loop do (let ((expr (read s nil)))
		 (if (not expr)
		     (return-from day13-part2 cost)
		     (let ((n (eval expr)))
		       (incf cost n))))))))

;;(day13-part2 "test.13.lisp")
;;(day13-part2 "input.13.lisp")


