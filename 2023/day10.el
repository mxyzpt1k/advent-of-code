;;; Day 10: Pipe Maze
;;; Advent of Code 2023
;;; Sunday, December 10, 2023

(defun day10-move (direction)
  (cl-case direction
    (east (forward-char))
    (west (backward-char))
    (otherwise (let ((x (- (point) (pos-bol))))
		 (if (eq 'south direction)
		     (forward-line)
		   (forward-line -1))
		 (beginning-of-line)
		 (forward-char x)))))

(defun day10-peek-char (direction)
  (save-excursion
    (day10-move direction)
    (get-byte)))

(defun day10-peek-pos (direction)
  (save-excursion
    (day10-move direction)
    (point)))

(defun day10-move-to-new (dir1 dir2 path)
  (let ((c (day10-peek-pos dir1)))
    (if (and (/= c (car path)) (/= c (cadr path)))
	(day10-move dir1)
      (day10-move dir2))))

(defun day10-follow-path (starting-point)
  (let ((path (list starting-point))
	(len 1))
    (catch 'ok
      (while (< len 100000)
	(cl-incf len)
	(push (point) path)
	(when (= (point) starting-point)
	  (throw 'ok path))
	(let ((b (get-byte)))
	  (cl-case b
	    (?| (day10-move-to-new 'north 'south path))
	    (?7 (day10-move-to-new 'west 'south path))
	    (?F (day10-move-to-new 'east 'south path))
	    (?J (day10-move-to-new 'west 'north path))
	    (?L (day10-move-to-new 'east 'north path))
	    (?- (day10-move-to-new 'east 'west path))
	    (?S (throw 'ok path))
	    (otherwise (cl-assert nil t "unexpected char: %c" b))))))))

(defun day10-part-1 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((pos (1- (search-forward "S"))))
      (goto-char pos)
      (cond ((not (= ?. (day10-peek-char 'north))) (day10-move 'north))
	    ((not (= ?. (day10-peek-char 'south))) (day10-move 'south))
	    ((not (= ?. (day10-peek-char 'east))) (day10-move 'east)))
      (let ((path (day10-follow-path pos)))
	(/ (length path) 2)))))

;;(day10-part-1 "test.buff")
;;(aoc-copy-output () (day10-part-1 "day10.2023.input.txt"))


;;; part 2

(defun day10-start-piece ()
  (beginning-of-buffer)
  (let ((pos (1- (search-forward "S")))
	(tile ()))
    (goto-char pos)
    (when (member (day10-peek-char 'north) '(?| ?7 ?F)) (push 'north tile))
    (when (member (day10-peek-char 'south) '(?| ?J ?L)) (push 'south tile))
    (when (member (day10-peek-char 'east) '(?- ?7 ?J)) (push 'east tile))
    (when (member (day10-peek-char 'west) '(?- ?F ?L)) (push 'west tile))
    (let ((S (cond ((equal tile '(south north)) ?|)
		   ((equal tile '(east north)) ?L)
		   ((equal tile '(west north)) ?J)
		   ((equal tile '(east south)) ?F)
		   ((equal tile '(west south)) ?7)
		   ((equal tile '(west east)) ?-)
		   (t (error "failed to find start piece")))))
      (delete-char 1)
      (insert S))))

(defun day10-expand-row (line)
  (let ((row (make-vector (1- (* 2 (length line))) ?+)))
    (dotimes (k (length line) row)
      (let ((c (aref line k)))
	(setf (aref row (* 2 k)) c)))))

(defun day10-expand-map (input)
  (let ((map (make-vector (1- (* 2 (length input))) nil)))
    (dotimes (k (length input) map)
      (setf (aref map (* 2 k)) (day10-expand-row (nth k input)))
      (unless (zerop k)
	(setf (aref map (1- (* 2 k))) (make-vector (1- (* 2 (length (car input)))) ?+))))))

(defun day10-get (map row col)
  (aref (aref map row) col))

(defun day10-get-list (map y rows x cols)
  (let ((acc ()))
    (dotimes (r rows (reverse acc))
      (dotimes (c cols)
	(push (day10-get map (+ y r) (+ x c)) acc)))))

(defun day10-set (map row col new)
  (let ((row (aref map row)))
    (setf (aref row col) new)))

(defun day10-set-list (map y rows x cols vals)
  (dotimes (r rows)
    (dotimes (c cols)
      (day10-set map (+ r y) (+ c x) (pop vals)))))


(defconst day10-horizontal-rules `((("FL-" "+" "-7J") (?* ?- ?*))))

(defconst day10-vertical-rules `((("|F7" "+" "|LJ") (?* ?| ?*))))

(defun day10-mark-passages (map)
  (let* ((rows (length map))
	 (cols (length (aref map 0)))
	 (count 0)
	 (set+ (lambda (map y x new) (cl-incf count) (day10-set map y x new))))
    ;; enclose the map
    (dotimes (y rows)
      (day10-set map y 0 ?#)
      (day10-set map y (1- cols) ?#))
    (dotimes (x cols)
      (day10-set map 0 x ?#)
      (day10-set map (1- rows) x ?#))
    (dotimes (y (+ -2 rows) count)
      (dotimes (x (+ -2 cols))
	(let ((abc (day10-get-list map y 1 x 3)))
	  (dolist (rule day10-horizontal-rules)
	    (let ((change (day10-apply-rule abc (car rule) (cadr rule))))
	      (when change
		(cl-incf count)
		(day10-set-list map y 1 x 3 change)))))
	(let ((abc (day10-get-list map y 3 x 1)))
	  (dolist (rule day10-vertical-rules)
	    (let ((change (day10-apply-rule abc (car rule) (cadr rule))))
	      (when change
		(cl-incf count)
		(day10-set-list map y 3 x 1 change)))))))))

(defun day10-match (c pat)
  "char - pattern"
  (or (string= pat "*") (seq-contains-p pat c)))

(defun day10-replace (abc replacement)
  (seq-mapn (lambda (a r)
	      (cond ((= r ?*) a)
		    (t r)))
	    abc replacement))
   
(defun day10-apply-rule (abc pattern replacement)
  (seq-let (a b c) abc
    (seq-let (p0 p1 p2) pattern
      (when (and (day10-match a p0) (day10-match b p1) (day10-match c p2))
	(day10-replace abc replacement)))))

(defun day10-display (map)
  (let ((b (get-buffer-create "day10.debug.map")))
    (with-current-buffer b
      (erase-buffer)
      (seq-do (lambda (row)
		(seq-do (lambda (char) (insert char)) row)
		(insert ?\n))
	      map))))

(defun day10-path-to-edge (map row col)
  (let ((path (list (cons row col)))
	(seen (make-hash-table :test 'equal))
	(edge nil))
    (while path
      (let* ((rc (pop path))
	     (row (car rc))
	     (col (cdr rc))
	     (p (day10-get map row col)))
	(when (not (gethash rc seen nil))
	  (setf (gethash rc seen) p)
	  (cond ((= p ?#) (setf edge t))
		((= p ?e) (setf edge t))
		;;((or (= p ?I) (= p ?O)))
		((or (= p ?+) (= p ?o) (= p ?.))
		 (push (cons (1+ row) col) path)
		 (push (cons (1- row) col) path)
		 (push (cons row (1+ col)) path)
		 (push (cons row (1- col)) path))))))
    (maphash (lambda (k v)
	       (cond ((= v ?.) (day10-set map (car k) (cdr k) (if edge ?O ?I)))
		     ((seq-contains "-|JLF7#" v))
		     (t (day10-set map (car k) (cdr k) (if edge ?e ?c)))))
	     seen)
    edge))
  
(defun day10-part-2 (buffer-name)
  (with-current-buffer buffer-name
    (day10-start-piece)
    (let* ((input (aoc-buffer-lines buffer-name))
	   (map (day10-expand-map input))
	   (count 0))
      (while (< 0 (day10-mark-passages map)) t)
      (dotimes (r (length map) count)
	(dotimes (c (length (aref map 0)))
	  (when (= ?. (day10-get map r c))
	    (day10-path-to-edge map r c))))
      (dotimes (r (length map) count)
	(dotimes (c (length (aref map 0)))
	  (when (= ?I (day10-get map r c))
	    (cl-incf count))))
      (day10-display map)
      (prog1
	  count
	(undo)))))

;; (day10-part-2 "test.buff")
;; (day10-part-2 "day10.2023.input.txt")
;(with-current-buffer "day10.2023.input.txt"  (day10-start-piece))
