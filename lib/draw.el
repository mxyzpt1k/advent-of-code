;;; simple turtle graphics for emacs
;;; started on Friday, December 29, 2023

(require 'cl-lib)
(require 'svg)

(cl-defstruct turtle buffer image direction color width fill x y shape)

(defun turtle-new (&optional buffer-name)
  (let ((name (or buffer-name "turtle.graphics")))
    (make-turtle :buffer (get-buffer-create name)
		 :image (svg-create 900 900)
		 :direction (turtle-eval-direction 'north)
		 :color 'black
		 :width 2
		 :fill nil
		 :x 450
		 :y 450
		 :shape nil)))

(defun turtle-icon (trtl)
  "draw the turtle, leaving the state as we found it"
  (let ((commands `((color green)
		    (fill green)
		    (width 1)
		    (left 90)
		    (forward 8)
		    (right 120)
		    (forward 16)
		    (right 120)
		    (forward 16)
		    (goto ,(turtle-x trtl) ,(turtle-y trtl))
		    (dir ,(turtle-direction trtl))
		    (width ,(turtle-width trtl))
		    (fill ,(turtle-fill trtl))
		    (color ,(turtle-color trtl)))))
    (turtle-exec trtl commands)))

(defun turtle-run (trtl commands)
  (with-current-buffer (turtle-buffer trtl)
    (dolist (cmd (turtle-compile commands))
      (erase-buffer)
      (turtle-exec trtl cmd)
      (insert-image (svg-image (turtle-image trtl)))
      (redisplay)			;redisplay should be in the drawing command
      (sleep-for 0.05))
    (turtle-icon trtl)
    (erase-buffer)
    (insert-image (svg-image (turtle-image trtl)))))

(defun turtle-compile (trtl commands)
  ;; instead of compiling, what if 'start is sort of an immediate mode?
  (let ((acc ()))
    (while commands
      (setq cmd (pop commands))
      (cl-case (car cmd)
	(start )
	(end )
	(otherwise (push cmd acc))))
    (reverse acc)))

(defun turtle-exec (trtl command)
  (cond ((consp (car command))
	 (seq-do (lambda (cmd) (turtle-exec trtl cmd)) command)
    (let ((arg (cadr command)))
      (cl-case (car command)
	(color (setf (turtle-color trtl) arg))
	(width (setf (turtle-width trtl) arg))
	(fill  (setf (turtle-fill trtl) arg))
	(right (setf (turtle-direction trtl) (- (turtle-direction trtl) arg)))
	(left  (setf (turtle-direction trtl) (+ (turtle-direction trtl) arg)))
	(dir (setf (turtle-direction trtl) (turtle-eval-direction arg)))
	(forward (turtle-forward trtl arg))
	(backward (turtle-forward trtl (- arg)))
	(goto (turtle-goto trtl (cadr command) (caddr command)))
	(start (setf (turtle-shape trtl) (cons (turtle-shape trtl) arg))) ;for polygons
	(otherwise (throw 'unknown-command (car command)))
	)))))

(defun turtle-eval-direction (arg)
  (if (numberp arg)
      arg
    (cl-case arg
      (east 0.0)
      (se 45.0)
      (south 90.0)
      (sw 135.0)
      (west 180.0)
      (nw 225.0)
      (north 270.0)
      (ne 315.0)
      (otherwise (throw 'unknown-direction arg)))))

(defun turtle-forward (trtl pixels)
  ;; one step is 4 pixels
  (let ((theta (degrees-to-radians (turtle-direction trtl))))
    (let ((dx (* pixels (cos theta)))
	  (dy (* pixels (sin theta))))
      (turtle-goto trtl (+ dx (turtle-x trtl)) (+ dy (turtle-y trtl))))))

(defun turtle-goto (trtl x2 y2)
  (let ((x1 (turtle-x trtl))
	(y1 (turtle-y trtl)))
    (svg-line (turtle-image trtl) x1 y1 x2 y2
	      :stroke-color (turtle-color trtl)
	      :stroke-width (turtle-width trtl))
    (setf (turtle-x trtl) x2 (turtle-y trtl) y2)))

(let ((trtl (turtle-new))
      (commands ()))
  (push `(forward 200) commands)
  (push `(right 120) commands)
  (push `(forward 200) commands)
  (push `(left 60) commands)
  (push `(backward 200) commands)
  (push `(color black) commands)
  (push `(goto 100 100) commands)
  (push `(color nil) commands)
  (dotimes (x 12)
    (push `(forward 200) commands)
    (push `(left 150) commands))
  (push `(color blue) commands)
  (push `(right 15) commands)
  (turtle-run trtl commands))

(turtle-run (turtle-new) `((dir sw) (forward 30)))

(dolist (day `(362 363 364 365))
  (insert (format "\nday %d, remaining %0.2f%%" day (* 100 (/ (- 365 day) 365.0)))))
day 362, remaining 0.82%
day 363, remaining 0.55%
day 364, remaining 0.27%
day 365, remaining 0.00%
