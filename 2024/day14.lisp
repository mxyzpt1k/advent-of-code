;;; Advent of Code
;;; Saturday, December 14, 2024

;;; This part 1 for the day

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  14   01:25:41   7153      0   10:23:14  18938      0

(defvar *height* 7)

(defvar *width* 11)

(defun quadrant (data)
  (destructuring-bind (px py vx vy) data
    (let* ((px (+ px (* 100 vx)))
	   (py (+ py (* 100 vy)))
	   (qx (mod px *width*))
	   (qy (mod py *height*))
	   (midx (floor (/ *width* 2)))
	   (midy (floor (/ *height* 2))))
      ;;(print `(,qx ,qy))
      (cond ((or (= qx midx) (= qy midy)) 0)
	    ((< qx midx) (if (< qy midy) 1 3))
	    (t (if (< qy midy) 2 4))))))

(defun day14-part1 (file)
  (let ((quads (vector 0 0 0 0 0)))
    (with-open-file (s file)
      (do ((data #1=(read s nil) #1#))
	  ((null data))
	(incf (aref quads (quadrant data)))))
    (reduce #'* (mapcar (lambda (i) (aref quads i)) `(1 2 3 4))
	    :initial-value 1)))

;;(day14-part1 "test.14")
;;(let ((*height* 103) (*width* 101)) (day14-part1 "input.14"))
