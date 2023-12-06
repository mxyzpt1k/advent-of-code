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

(defun day6-part-2 (buffer-name)
  (day6-count-wins 50748685 242101716911252))

(defun day6-count-wins ()
  (let ((wins 0))
    (dotimes (speed time wins)
      (when (> (* speed (- time speed)) record)
	(setf wins (1+ wins))))))

;; (aoc-copy-output () (day6-part-1))
;; (aoc-copy-output () (day6-part-2))

