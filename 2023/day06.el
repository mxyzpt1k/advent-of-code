;;; Day 6: Wait For It
;;; Advent of Code 2023
;;; Wednesday, December 06, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   6   00:25:28   8174      0   00:29:28   7023      0

(defun day6-part-1 ()
  (let ((times `(50     74     86     85))
	(dists `(242   1017   1691   1252)))
    (seq-reduce #'* (cl-mapcar #'day6-count-wins times dists) 1)))

(defun day6-part-2 ()
  (day6-count-wins 50748685 242101716911252))

(defun day6-count-wins (time record)
  (let ((wins 0))
    (dotimes (speed time wins)
      (when (> (* speed (- time speed)) record)
	(setf wins (1+ wins))))))

;; (aoc-copy-output () (day6-part-1))
;; (benchmark-run (aoc-copy-output () (day6-part-2)))
;;  => 84 seconds

(defun day6-count-wins-afterwards (time record)
  ;; turns out we're done when we find the first win because the
  ;; solutions are symmetrical
  ;;   d = v(t - v)
  (catch 'wins
    (dotimes (speed time)
      (let ((time-left (- time speed)))
	(when (> (* speed time-left) record)
	  (throw 'wins (1+ (- time-left speed))))))))

;; (benchmark-run (aoc-copy-output () (day6-count-wins-afterwards 50748685 242101716911252)))
;;  => 9 seconds

(defun day6-count-wins-afterwards2 (time record)
  ;; Even faster, we can use the quadratic formula since
  ;;   d = v(t - v) = tv - v^2
  ;;   v^2 - tv + d = 0
  ;; the two roots should be both close to the solution
  (let ((a 1)
	(b time)
	(c record))
    (cl-flet ((qf (p/m) (- (/ (funcall p/m (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))
	      (tst (v) (* v (- time v))))
      (let* ((v (- (floor (qf #'+)) 4)) ; the smaller root + some margin
	     (d (tst v)))
	(while (<= d record)
	  (setq v (1+ v))
	  (setq d (tst v)))
	(1+ (- (- time v) v))))))

;; (benchmark-run (day6-count-wins-afterwards2 50748685 242101716911252)  )
;;  => 5.4e-5 seconds
