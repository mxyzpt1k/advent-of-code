;;; Day 21: Step Counter
;;; Advent of Code 2023
;;; Thursday, December 21, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  21   03:28:24   7982      0          -      -      -

(defun day21-part1 (buffer-name steps-wanted)
  (with-current-buffer buffer-name
    (let ((lines (mapcar 'vconcat (aoc-buffer-lines buffer-name))))
      (let ((grid (aoc-add-border (seq-into lines 'vector) ?%)))
	(let ((memo (make-hash-table :test 'equal))
	      (start (day21-start grid))
	      (count 0))
	  (day21-mark-connected grid start 0 memo)
	  (day21-count-steps)
	  ;;(message "%s" memo)
	  count)))))

;; (day21-part1 "day21.2023.input.txt" 64)
;; (day21-part1 "test.buff" 6)

(defun day21-mark-connected (grid start distance hashtab)
  ;; added hashtab for part 2
  (setf (gethash start hashtab) distance)
  (dotimes (k (1+ steps-wanted))
    (maphash (lambda (pos dist)
	       (when (= k dist) (day21-mark-helper grid pos (1+ dist) hashtab)))
	     hashtab)))

(defun day21-mark-helper (grid start distance hashtab)
  (dolist (dir '((-1 0) (1 0) (0 -1) (0 1)))
    (let ((pos (seq-mapn #'+ start dir)))
      (cond ((not (day21-garden-p grid pos)))
	    ((gethash pos hashtab))
	    (t (setf (gethash pos hashtab) distance))))))
	    
(defun day21-count-steps ()
  (day21-walk grid (lambda (row col chr)
		(let* ((pos (list row col))
		       (dist (gethash pos memo (1+ steps-wanted))))
		  (when (and (cl-evenp dist)
			     (<= dist steps-wanted))
		    (cl-incf count))))))

(defun day21-walk (grid func)
  (dotimes (row (length grid))
    (dotimes (col (length (aref grid 0)))
      (funcall func row col (aref (aref grid row) col)))))
  
(defun day21-start (grid)
  (catch 'start
    (day21-walk grid (lambda (row col char)
		       (when (= ?S char)
			 (throw 'start (list row col)))))))

(defun day21-garden-p (grid pos)
  ;; updated in part 2 to check bounds instead of adding a border
  (let ((c (day21-get grid pos)))
    (or (= ?. c) (= ?S c))))

(defun day21-get (grid pos)
  ;; added for part 2
  (seq-let (row col) pos
    (cond ((< row 0) ?%)
	  ((< col 0) ?%)
	  ((not (< row (length grid))) ?%)
	  ((not (< col (length (aref grid 0)))) ?%)
	  (t (aref (aref grid row) col)))))


;;; part 2 - an infinite map, of course

(defun day21-part2 (buffer-name steps-wanted)
  (with-current-buffer buffer-name
    (let* ((grid (vconcat (mapcar 'vconcat (aoc-buffer-lines buffer-name))))
	   ;; lots to keep track of
	   (memo (make-hash-table :test 'equal))
	   (N (make-hash-table :test 'equal))
	   (S (make-hash-table :test 'equal))
	   (E (make-hash-table :test 'equal))
	   (W (make-hash-table :test 'equal))
	   (NE (make-hash-table :test 'equal))
	   (NW (make-hash-table :test 'equal))
	   (SE (make-hash-table :test 'equal))
	   (SW (make-hash-table :test 'equal))
	   (count 0)
	   (start (day21-start grid)))
      (message "starting at %s" start)
      (let ((steps-wanted (+ (car start) (cadr start))) ;start is at the center
	    (X (1- (length (aref grid 0))))
	    (Y (1- (length grid))))
	(day21-mark-connected grid start 0 memo)
	(day21-mark-connected grid (list 0 0) 0 NW)
	(day21-mark-connected grid (list 0 X) 0 NE)
	(day21-mark-connected grid (list Y 0) 0 SW)
	(day21-mark-connected grid (list Y X) 0 SE)
	(day21-mark-connected grid (list (car start) X) 0 E)
	(day21-mark-connected grid (list (car start) 0) 0 W)
	(day21-mark-connected grid (list 0 (cadr start)) 0 N)
	(day21-mark-connected grid (list Y (cadr start)) 0 S))
      (day21-walk grid #'day21-count-steps2)
      count
      )))

(defun day21-count-steps2 (row col chr)
  ;; called from day21-walk-grid
  (let ((pos (list row col))
	(mid (car start))		;not very general purpose -- next time
	(size (length grid))
	(default (+ 1 steps-wanted)))
    (let ((dist (gethash pos memo default))
	  (dist-N (gethash pos N default))
	  (dist-S (gethash pos S default))
	  (dist-E (gethash pos E default))
	  (dist-W (gethash pos W default))
	  (dist-NE (gethash pos NE default))
	  (dist-NW (gethash pos NW default))
	  (dist-SE (gethash pos SE default))
	  (dist-SW (gethash pos SW default)))
      (day21-tally dist)
      (day21-tally-line (+ dist-E 1 mid))	; go west young man
      (day21-tally-line (+ dist-W 1 mid))
      (day21-tally-line (+ dist-S 1 mid))
      (day21-tally-line (+ dist-N 1 mid))
      (day21-tally-corner (+ dist-SW 1 size))
      (day21-tally-corner (+ dist-NW 1 size))
      (day21-tally-corner (+ dist-NE 1 size))
      (day21-tally-corner (+ dist-SE 1 size))
      )))

(defun day21-tally (steps)
  (when (and (cl-evenp steps)
	     (<= steps steps-wanted))
    (cl-incf count)))

(defun day21-tally-line (steps)
  (let ((x (/ (- steps-wanted steps) size)))
    (cl-incf count x)))

(defun day21-tally-corner (steps)
  (let ((x  (/ (- steps-wanted steps) size)))
    (cl-incf count (/ (* x (1+ x)) 2))))

;; (day21-part2 "test.buff" 6)
;; (aoc-copy-output () (day21-part2 "day21.2023.input.txt" 26501365))

;; 309232180270295 is too low
;; 309235237462497 is too low
;; 311076884615997 is too low
;; 620306030343511 is not right

(defun day21-grid-extent (grid start)
  (let ((height (length grid))
	(width (length (aref grid 0))))
    (list (car start)
	  (- height (car start))
	  (cadr start)
	  (- width (cadr start)))))

(defun day21-dump-grid ()
  (let ((grid (day21-part2 "test.buff")))
    (day21-walk grid (lambda (r c chr)
		       (when (zerop c) (insert "\n"))
		       (insert chr)))))

