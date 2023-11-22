;;; day14.el - advent of code 2015
;;; Tuesday, November 21, 2023
;;; warming up for 2023

(defun deer-distance (limit speed time wait)
  (let ((whole (/ limit (+ wait time)))
	(part (mod limit (+ wait time))))
    (+ (* whole speed time) (* speed (min time part)))))

(defun deer-running-p (second time wait)
  (< (mod second (+ time wait)) time))

;; (deer-running-p 9 8 11)
;; (deer-running-p 8 8 11)
;; (deer-running-p 7 8 11)
;; (deer-running-p 19 8 11)

(defun deer-distance2 (limit speed time wait)
  (let ((seconds ())
	(distance 0)
	(state 'run))
    (dotimes (sec limit (reverse seconds))
      (if (deer-running-p sec time wait)
	  (push (cl-incf distance speed) seconds)
	(push distance seconds)))))

;;(reverse (deer-distance2 10 5 8 7))

(defun deer-winner (deer-times)
  (let ((points (make-vector (length deer-times) 0)))
    (dotimes (second (length (car deer-times)))
      (let ((best 0)
	    (best-idx 0)
	    (idx 0))
	(dolist (deer deer-times)
	  (if (< best (aref deer second))
	      (setf best (aref deer second))))
	(dolist (deer deer-times)
	  (if (= best (aref deer second))
	      (cl-incf (aref points idx)))
	  (cl-incf idx))))
    (apply #'max (seq-map #'identity points))))

;; (aoc-copy-output () (day14 1000 "day14.test.input"))
;; (aoc-copy-output () (day14 2503 "day14.input.txt"))

(defun day14 (time-limit input-buffer)
  (let ((best 0)
	(times ()))
    (dolist (line (aoc-buffer-lines input-buffer) best)
      (let ((words (split-string line)))
	(cl-destructuring-bind
	    (deer can fly speed km/s for time seconds but then must rest for wait seconds) words
	  (let ((distance (deer-distance2
			   time-limit
			   (string-to-number speed)
			   (string-to-number time)
			   (string-to-number wait))))
	    ;; part 1
	    ;;(if (> distance best) (setq best distance))
	    ;; part 2
	    (push distance times)
	    ))))
    (deer-winner (mapcar (lambda (s) (apply #'vector s)) times))
    ))

