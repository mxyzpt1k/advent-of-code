;;;
;;; Advent of Code 2023
;;; Saturday, December 16, 2023

(cl-defstruct city-block value row col north south east west char)

(defun init-city-block (grid row col)
  (let (char (day10-get grid row col))
    (make-city-block :value (- char ?0)
		     :char char
		     :north 1000000
		     :south 1000000
		     :east 1000000
		     :west 1000000
		     :row row
		     :col col)))

(defun city-edge-p (b)
  (= ?# (city-block-char b)))

(defun day17-walk (grid fun)
  (dotimes (r (length grid))
    (dotimes (c (length (aref grid r)))
      (funcall fun grid r c))))
 
(defun day17-part-1 (buffer-name)
  (let ((lines (mapcar (lambda (line) (seq-into line 'vector))
		       (aoc-buffer-lines buffer-name))))
    (let* ((grid (aoc-add-border (seq-into lines 'vector)))
	   (memo (make-hash-table :test 'equal))
	   (loss (- (day17-value grid 1 1))))
      (minimize-heat-loss grid)
      *best-score*)))

(defun day17-score (num)
  (when (< num *best-score*)
    (message "score: %d, path: %d" num (length path))
    (day17-display grid path)
    (setq *best-score* num)))

(defun n-in-a-row-p (n path next-fun)
  (cond ((null path) nil)
	((= 1 n) t)
	((equal (funcall next-fun (car path)) (cadr path))
	 (n-in-a-row-p (1- n) (cdr path) next-fun))
	(t nil)))

(defun day17-row-incr (pair) (cons (1+ (car pair)) (cdr pair)))

(defun day17-row-decr (pair) (cons (1- (car pair)) (cdr pair)))

(defun day17-col-incr (pair) (cons (car pair) (1+ (cdr pair))))

(defun day17-col-decr (pair) (cons (car pair) (1- (cdr pair))))

(defun day17-valid-path-p (path)
  (cond ((null (cddr path)) t)
	((equal (car path) (caddr path)) nil) ;reversed
	((n-in-a-row-p 4 path 'day17-row-incr) nil)
	((n-in-a-row-p 4 path 'day17-row-decr) nil)
	((n-in-a-row-p 4 path 'day17-col-decr) nil)
	((n-in-a-row-p 4 path 'day17-col-incr) nil)
	(t t)))

;;(day17-valid-path-p '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (2 . 4)))

(defun day17-display (grid path)
  (let ((b (get-buffer-create "day17.debug")))
    (with-current-buffer b
      (erase-buffer)
      (dotimes (r (length grid))
	(dotimes (c (length (aref grid 0)))
	  (if (member (cons r c) path)
	      (insert ?@)
	    (insert (day10-get grid r c))))
	(insert ?\n)))))

(defun day17-value (grid row col)
  (- (aref (aref grid row) col) ?0))

(defun day17-test-path (grid loss path)
  ;; an initial path for others to beat
  (let ((last-row (+ -2 (length grid)))
	(last-col (+ -2 (length (aref grid 1))))
	(row 1)
	(col 1)
	(done nil))
    (while (not done)
      (let ((c (day17-value grid row col)))
	(push (cons row col) path)
	(cl-incf loss c)
	(if (and (= row last-row) (= col last-col))
	    (setf done t)
	  (if (> row col)
	      (cl-incf col)
	    (cl-incf row)))))
    (day17-score loss)))

(defun minimize-heat-loss (grid)
  ;; excessive recursion
  (let ((memo (make-hash-table :test equal))
	(dir 'east)
	(path (list (cons (cons 1 1) 'east)))
	(loss (- (day17-value grid 1 1)))
	(row 1)
	(col 1)
	(last-row (+ -2 (length grid)))
	(last-col (+ -2 (length (aref grid 1))))
	(done nil))
    (day17-test-path grid loss ())
    (while (not done)
      (let ((c (day17-value grid row col))
	    (pair (cons rol col)))
	(cl-incf loss c)
	(push pair path)
	(if (and (= row last-row) (= col last-col) (day17-valid-path-p path))
	    (day17-score loss)
	  (let* ((key (cons pair dir)))
	    (when (and (< loss *best-score*)
		       (/= ?# (day10-get grid row col))
		       (<= loss (gethash key memo loss))
		       (day17-valid-path-p path))
	      (setf (gethash key memo) loss)
	      (unless (eq 'south dir)
		(minimize-heat-loss grid (1+ row) col val (cons pair path) memo 'north))
	      (unless (eq 'east dir)
		(minimize-heat-loss grid row (1+ col) val (cons pair path) memo 'west))
	      (unless (eq 'north dir)
		(minimize-heat-loss grid (1- row) col val (cons pair path) memo 'south))
	      (unless (eq 'west dir)
		(minimize-heat-loss grid row (1- col) val (cons pair path) memo 'east))
	      )))))))

;; (day17-part-1 "test.buff")
;; (day17-part-1 "day17.2023.input.txt")

;; 1412 is too high
