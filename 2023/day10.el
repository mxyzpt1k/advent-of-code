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
  ;; figure out what the start piece is
  (beginning-of-buffer)
  (let ((pos (1- (search-forward "S")))
	(tile ()))
    (goto-char pos)
    (when (member (day10-peek-char 'north) '(?| ?7 ?F)) (push 'north tile))
    (when (member (day10-peek-char 'south) '(?| ?J ?L)) (push 'south tile))
    (when (member (day10-peek-char 'east) '(?- ?7 ?J)) (push 'east tile))
    (when (member (day10-peek-char 'west) '(?- ?F ?L)) (push 'west tile))
    (cond ((equal tile '(south north)) ?|)
	  ((equal tile '(east north)) ?L)
	  ((equal tile '(west north)) ?J)
	  ((equal tile '(east south)) ?F)
	  ((equal tile '(west south)) ?7)
	  ((equal tile '(west east)) ?-)
	  (t (error "failed to find start piece")))))

;; (with-current-buffer "day10.2023.input.txt" (format "%c" (day10-start-piece)))

(defun day10-get (map row col)
  (aref (aref map row) col))

(defun day10-set (map row col new)
  (let ((row (aref map row)))
    (setf (aref row col) new)))

(defun day10-char-points (char)
  (cl-case char
    (?| '((0 1) (1 1) (2 1)))
    (?- '((1 0) (1 1) (1 2)))
    (?7 '((1 0) (1 1) (2 1)))
    (?F '((1 1) (1 2) (2 1)))
    (?L '((0 1) (1 1) (1 2)))
    (?J '((0 1) (1 1) (1 0)))
    (?. '((1 1)))
    (otherwise (error "unexpected char"))))

(defun day10-expand-char (map row col char)
  (if (= char ?S)
      (day10-expand-char map row col (day10-start-piece))
    (dolist (point (day10-char-points char))
      (seq-let (r c) point
	(day10-set map (+ r (* 3 row)) (+ c (* 3 col))
		   (if (= ?. char) ?. ?x))))))

(defun day10-add-border (map)
  (let ((cols (length (aref map 0)))
	(rows (length map)))
    (let ((new (make-vector (+ 2 rows) ())))
      (setf (aref new 0) (make-vector (+ 2 cols) ?#))
      (setf (aref new (1+ rows)) (make-vector (+ 2 cols) ?#))
      (dotimes (r rows new)
	(setf (aref new (1+ r)) (vconcat [?#] (aref map r) [?#]))))))

(defun day10-mark-path (map row col)
  (dotimes (r 3)
    (dotimes (c 3)
      (when (= ?x (day10-get map (+ r (* row 3)) (+ c (* col 3))))
	(day10-set map (+ r (* row 3)) (+ c (* col 3)) ?%)))))
    
(defun day10-expand (map path)
  (let ((new (make-vector (* 3 (length map)) ()))
	(cols (length (aref map 0))))
    (dotimes (row (length map))
      (dotimes (r 3)
	(setf (aref new (+ r (* 3 row))) (make-vector (* 3 cols) ?o)))
      (dotimes (col cols)
	(day10-expand-char new row col (day10-get map row col))))
    (dolist (rc path)
      (day10-mark-path new (car rc) (cdr rc)))
    (day10-add-border new)))

(defun day10-display (map)
  (let ((b (get-buffer-create "day10.debug.map")))
    (with-current-buffer b
      (erase-buffer)
      (seq-do (lambda (row)
		(seq-do (lambda (char) (insert char)) row)
		(insert ?\n))
	      map))))

(defun day10-find-edge (map key)
  (let ((path (list key))
	(seen (make-hash-table :test 'equal))
	(outside nil))
    (while path
      (let* ((p (pop path))
	     (row (car p))
	     (col (cdr p)))
	(let ((c (day10-get map row col)))
	  (cond ((gethash p seen))
		((= c ?#) (setf outside t))  ; hit a wall
		((= c ?%))		     ; hit a pipe
		((= c ?e) (setf outside t))  ; previously seen outside
		((= c ?i))		     ; previously seen inside
		(t (push (cons (1- row) col) path)
		   (push (cons (1+ row) col) path)
		   (push (cons row (1+ col)) path)
		   (push (cons row (1- col)) path)))
	  (setf (gethash p seen) c))))
    (maphash (lambda (k v)
	       (unless (or (= v ?#) (= v ?%))
		 (day10-set map (car k) (cdr k) (if outside ?e ?i))))
	     seen)))

(defun day10-score (map big)
  (let ((score 0))
    (dotimes (row (length map) score)
      (dotimes (col (length (aref map 0)))
	(let ((enclosed 0))
	  (dotimes (r 3)
	    (dotimes (c 3)
	      (when (= ?i (day10-get big (+ r (* row 3)) (+ c (* col 3))))
		(cl-incf enclosed))))
	  (when (= 9 enclosed)
 	    (cl-incf score)))))))

(defun day10-get-path ()
  (beginning-of-buffer)
  (let ((pos (1- (search-forward "S"))))
    (goto-char pos)
    (day10-move 'north)			;it' a 7
    (day10-follow-path pos)))

(defun day10-convert-to-yx (pos)
  (cons (/ pos *line-length*) (1- (mod pos *line-length*))))

(defun day10-find-start ()
  (beginning-of-buffer)
  (let ((pos (1- (search-forward "S"))))
    (day10-convert-to-yx pos)))

(defun day10-part-2 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((*line-length* (line-end-position)))
      (let* ((map (seq-into (aoc-buffer-lines buffer-name) 'vector))
	     (path (mapcar 'day10-convert-to-yx (day10-get-path)))
	     (big (day10-expand map path)))
	(let ((rc (day10-find-start)))
	  (cl-assert (= ?S (day10-get map (car rc) (cdr rc))) t))
	;;(day10-display big)))))
	(dotimes (r (length big))
	  (dotimes (c (length (aref big 0)))
	    (when (= ?x (day10-get big r c))
	      (day10-find-edge big (cons r c)))))
	;;(day10-display big)
	(day10-score map big)))))

;; (day10-part-2 "test.buff")
;; (aoc-copy-output () (day10-part-2 "day10.2023.input.txt"))

