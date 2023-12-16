;;; Day 16: The Floor Will Be Lava
;;; Advent of Code 2023
;;; Friday, December 15, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  16   03:12:06   7575      0   03:38:44   7381      0

(cl-defstruct beam direction row col active)

(cl-defstruct tile func north south east west)

(defun day16-make-tile (func)
  (make-tile :func func :north nil :south nil :east nil :west nil))

(defun tile-energized-p (tile)
  (or (tile-north tile)
      (tile-south tile)
      (tile-east tile)
      (tile-west tile)))

(defun day16-vertical (beam)
  (let ((new nil))
    (cl-case (beam-direction beam)
      ((north south) (day16-forward beam))
      (otherwise
       (setf new (make-beam :direction 'north :row (1- (beam-row beam)) :col (beam-col beam) :active t))
       (cl-incf (beam-row beam))
       (setf (beam-direction beam) 'south)))
    new))

(defun day16-horizontal (beam)
  (let ((new nil))
    (cl-case (beam-direction beam)
      ((east west) (day16-forward beam))
      (otherwise
       (setf new (make-beam :direction 'east :row (beam-row beam) :col (1+ (beam-col beam)) :active t))
       (setf (beam-direction beam) 'west)
       (cl-incf (beam-col beam) -1)))
    new))

(defun day16-slash (beam)		; / 
  (prog1 nil
    (cl-case (beam-direction beam)
      (north (setf (beam-direction beam) 'east)
	     (cl-incf (beam-col beam)))
      (south (setf (beam-direction beam) 'west)
	     (cl-incf (beam-col beam) -1))
      (east (setf (beam-direction beam) 'north)
	    (cl-incf (beam-row beam) -1))
      (west (setf (beam-direction beam) 'south)
	    (cl-incf (beam-row beam))))))

(defun day16-backslash (beam)		; \
  (prog1 nil
    (cl-case (beam-direction beam)
      (north (setf (beam-direction beam) 'west)
	     (cl-incf (beam-col beam) -1))
      (south (setf (beam-direction beam) 'east)
	     (cl-incf (beam-col beam)))
      (west (setf (beam-direction beam) 'north)
	    (cl-incf (beam-row beam) -1))
      (east (setf (beam-direction beam) 'south)
	    (cl-incf (beam-row beam))))))

(defun day16-forward (beam)
  (prog1 nil
    (cl-case (beam-direction beam)
      (north (cl-incf (beam-row beam) -1))
      (south (cl-incf (beam-row beam)))
      (east (cl-incf (beam-col beam)))
      (west (cl-incf (beam-col beam) -1))
      (t (cl-assert "unexpected direction")))))
  
(defun day16-make-grid (buffer-name)
  ;; watch out for a little code from day10
  (let ((lines (mapcar (lambda (line) (seq-into line 'vector))
		       (aoc-buffer-lines buffer-name))))
    (let ((grid (day10-add-border (seq-into lines 'vector))))
      (dotimes (r (length grid) grid)
	(setf (aref grid r) (seq-into (seq-map 'day16-make-tile (aref grid r)) 'vector))))))

(defun day16-advance (grid beam)
  (let ((tile (aref (aref grid (beam-row beam)) (beam-col beam))))
    (cl-case (beam-direction beam)
      (north (if (tile-north tile)
		 (setf (beam-active beam) nil)
	       (setf (tile-north tile) t)))
      (south (if (tile-south tile)
		 (setf (beam-active beam) nil)
	       (setf (tile-south tile) t)))
      (east (if (tile-east tile)
		(setf (beam-active beam) nil)
	      (setf (tile-east tile) t)))
      (west (if (tile-west tile)
		(setf (beam-active beam) nil)
	      (setf (tile-west tile) t))))
    (cl-case (tile-func tile)
      (?# (setf (beam-active beam) nil))
      (?. (day16-forward beam))
      (?- (day16-horizontal beam))
      (?| (day16-vertical beam))
      (?/ (day16-slash beam))
      (?\\ (day16-backslash beam)))))

(defun day16-score (grid)
  (let ((score 0))
    (dotimes (r (length grid) score)
      (dotimes (c (length (aref grid r)))
	(let ((tile (aref (aref grid r) c)))
	  (when (tile-energized-p tile)
	    (unless (= ?# (tile-func tile))
	      (cl-incf score))))))))

(defun day16-display (grid)
  ;; for debugging
  (let ((b (get-buffer-create "day16.debug.buff")))
    (with-current-buffer b
      (dotimes (r (length grid))
	(dotimes (c (length (aref grid 0)))
	  (let ((tile (aref (aref grid r) c)))
	    (cond ((= ?# (tile-func tile)) (insert " ")) 
		  ((tile-energized-p tile) (insert "@"))
		  (t (insert (tile-func tile))))))
	(insert "\n")))))

(defun day16-part-1 (buffer-name)
  (let* ((grid (day16-make-grid buffer-name))
	 (beams (list (make-beam :direction 'east :row 1 :col 1 :active t))))
    (while beams
      ;;(message (format "%d beams" (length beams)))
      (let ((acc ()))
	(dolist (beam beams)
	  (when (beam-active beam)
	    (push beam acc)
	    (let ((new (day16-advance grid beam)))
	      (when new
		(push new acc)))))
	(setq beams acc)))
    ;;(day16-display grid)
    (day16-score grid)))

;; (day16-part-1 "test.buff")
;; (aoc-copy-output () (day16-part-1 "day16.2023.input.txt"))


;;; part 2

(defun day16-reset (grid)
  (seq-map (lambda (row)
	     (seq-map (lambda (tile)
			(setf (tile-north tile) nil (tile-south tile) nil)
			(setf (tile-east tile) nil (tile-west tile) nil))
		      row))
	   grid))

(defun day16-trial (grid first-beam)
  ;; this is the part 1 solution with a variable starting point
  (day16-reset grid)
  (let ((beams (list first-beam)))
    (while beams
      (let ((acc ()))
	(dolist (beam beams)
	  (when (beam-active beam)
	    (push beam acc)
	    (let ((new (day16-advance grid beam)))
	      (when new
		(push new acc)))))
	(setq beams acc)))
    (day16-score grid)))

(defun day16-part-2 (buffer-name)
  (let ((grid (day16-make-grid buffer-name)))
    (let ((rows (length grid))
	  (cols (length (aref grid 0)))
	  (best 0))
      (dotimes (r (- rows 2))		;-2 because the grid has a border
	(setq best (max best (day16-trial grid (make-beam :direction 'east :row (1+ r) :col 1 :active t))))
	(setq best (max best (day16-trial grid (make-beam :direction 'west :row (1+ r) :col (1- cols) :active t)))))
      (dotimes (c (- cols 2))
	(setq best (max best (day16-trial grid (make-beam :direction 'south :row 1 :col (1+ c) :active t))))
	(setq best (max best (day16-trial grid (make-beam :direction 'north :row (1- rows) :col (1+ c) :active t)))))
      best)))

;; (day16-part-2 "test.buff")
;; (aoc-copy-output () (day16-part-2 "day16.2023.input.txt"))
