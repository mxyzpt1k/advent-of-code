;;; Day 17: Clumsy Crucible
;;; Advent of Code 2023
;;; Saturday, December 16, 2023

(defun day17-get (grid row col)
  (aref (aref grid row) col))

(defun day17-value (grid row col)
  (- (aref (aref grid row) col) ?0))

(defun day17-set (grid row col val)
  (let ((r (aref grid row)))
    (setf (areg grid col) val)))

(defun day17-display ()
  (let ((b (get-buffer-create "day17.debug"))
	(g (make-hash-table :test 'equal)))
    (with-current-buffer b
      (erase-buffer)
      (seq-mapn (lambda (p dir)
		  (setf (gethash p g) (symbol-name dir)))
		path dir-path)
      (dotimes (r (length grid))
	(dotimes (c (length (aref grid 0)))
	  (insert (gethash (cons r c) g (day17-get grid r c))))
	(insert ?\n)))))

(defun day17-score (num)
  (when (< num *best-score*)
    (day17-display)
    (message "score: %d, path: %d" num (length path))
    (setq *best-score* num)))

(defun day17-test-path (grid row col loss)
  ;; an initial path for others to beat
  (let ((done nil)
	(path ()))
    (while (not done)
      (let ((c (day17-value grid row col)))
	(push (cons row col) path)
	(cl-incf loss c)
	(if (> row col)
	    (cl-incf col)
	  (cl-incf row)))
      (when (and (= row *last-row*) (= col *last-col*))
	(setf done t)))
    ;;(message "%s" path)
    (day17-score loss)))

(defun day17-part-1 (buffer-name)
  (let ((lines (mapcar (lambda (line) (seq-into line 'vector))
		       (aoc-buffer-lines buffer-name)))
	(*best-score* 999999999)
	(dir-path ()))
    (let* ((grid (aoc-add-border (seq-into lines 'vector)))
	   (loss (- (day17-value grid 1 1)))
	   (*memo* (make-hash-table :test 'equal))
	   (*last-row* (+ -2 (length grid)))
	   (*last-col* (+ -2 (length (aref grid 0)))))
      ;; #'day17-test-path sets *best-score* to something reasonable
      (day17-test-path grid 1 1 loss)
      ;;(setq *best-score* 103)
      (minimize-heat-loss grid 1 1 loss (list (cons 1 1)) (list 'v))
      (message "h-t-c: %s" (hash-table-count *memo*))
      *best-score*))))

(defun day17-valid-path-p (dir-path)
  (let ((four (seq-take dir-path 4)))
    (if (< (length four) 4)
	t
      (not (seq-every-p (lambda (dir) (eq dir (car four))) four)))))

(defun minimize-heat-loss (grid row col loss path dir-path)
  (unless (= ?# (day17-get grid row col))
    (let* ((c (day17-value grid row col))
	   (from-dir (car dir-path))
	   (pair (cons row col))
	   (key (list pair from-dir))
	   (new-path (cons pair path)))
      (when (and (< loss *best-score*)
		 (< loss (+ 7 (gethash key *memo* (1+ loss))))
		 (day17-valid-path-p dir-path))
	(if (and (= row *last-row*) (= col *last-col*))
	    (let ((path (cons (cons row col) path)))
	      (day17-score (+ c loss)))
	  (when (< loss (gethash key *memo* (1+ loss)))
	    (setf (gethash key *memo*) loss))
	  (unless (eq '< from-dir)
	    (minimize-heat-loss grid row (1+ col) (+ c loss) new-path (cons '> dir-path)))
	  (unless (eq '^ from-dir)
	    (minimize-heat-loss grid (1+ row) col (+ c loss) new-path (cons 'v dir-path)))
	  (unless (eq '> from-dir)
	    (minimize-heat-loss grid row (1- col) (+ c loss) new-path (cons '< dir-path)))
	  (unless (eq 'v from-dir)
	    (minimize-heat-loss grid (1- row) col (+ c loss) new-path (cons '^ dir-path)))
	  )))))

;; (day17-part-1 "test.buff")

;; (day17-part-1 "day17.2023.input.txt")

;; 1412 is too high
;;  896 is too high
;;  890 is too high
;;  613 is not the answer

(* 4 141 141)

(* 4 13 13)

(* 13 13 4)
