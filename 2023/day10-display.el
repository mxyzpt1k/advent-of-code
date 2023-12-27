;;; Day 10: Pipe Maze
;;; Advent of Code 2023
;;; Sunday, December 10, 2023

;;; trying to animate day 10, part 2
;;; Tuesday, December 26, 2023
;;;
;;; not very efficient, i ended up writing out image files then putting
;;; them together in iMovie

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
  (aoc-add-border map ?B))

(defun day10-mark-path (map row col)
  (dotimes (r 3)
    (dotimes (c 3)
      (when (= ?x (day10-get map (+ r (* row 3)) (+ c (* col 3))))
	(day10-set map (+ r (* row 3)) (+ c (* col 3)) ?P)))))
    
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

(defun day10-display-orig (map)
  (let ((b (get-buffer-create "day10.debug.map")))
    (with-current-buffer b
      (erase-buffer)
      (seq-do (lambda (row)
		(seq-do (lambda (char) (insert char)) row)
		(insert ?\n))
	      map))))

(defun day10-color (c) 
  (cond ((= c ?B) "0 0 0\n")			; hit a wall
	((= c ?P) "0 0 7\n")			; hit a pipe
	((= c ?e) "5 2 4\n")			; previously seen outside
	((= c ?i) "4 4 4\n")			; previously seen inside
	((= c ?s) "7 5 0\n")			; interior space
	(t "0 0 4\n")))

(defun day10-update-image (map row col)
  (cl-assert (numberp row))
  (cl-assert (numberp col))
  (let ((color (day10-get map row col))
	(index (+ col (* row (length (aref map 0))))))
    (setf (aref *image-data* index) color)))
  ;; (let ((color (day10-color (day10-get map row col)))
  ;; 	(row-len (length (aref map 0)))
  ;; 	(offsets '((0 . 0) (0 . 1) (0 . 2)
  ;; 		   (1 . 0) (1 . 1) (1 . 2)
  ;; 		   (2 . 0) (2 . 1) (2 . 2))))
  ;;   (dolist (yx offsets)
  ;;     (let ((idx (+ (* row-len (+ row (car yx))) col (cdr yx))))
  ;; 	(setf (aref *image-data* idx) color)))))

(defun day10-init-image (map)
  (dotimes (row (length map))
    (dotimes (col (length (aref map 0)))
      (day10-update-image map row col))))
  
(defun day10-display (map &optional row col)
  ;; current buffer should be day10.ppm
  (with-current-buffer (get-buffer-create "day10.output.ppm")
    (buffer-disable-undo)
    (fundamental-mode)
    (erase-buffer)
    (let ((rows (length map))
	  (cols (length (aref map 0))))
      (insert (day10-ppm-header cols rows))
      (dotimes (row (length map))
	(dotimes (col (length (aref map 0)))
	  (insert (day10-color (day10-get map row col))))))))

(defun day10-write (map &optional row col)
  ;; current buffer should be day10.ppm
  (cl-incf *image-count*)
  (when (zerop (mod *image-count* 100))
    (let ((default-directory "~/devel/advent-of-code/2023/day10/"))
      (with-existing-directory 
	(let ((name (format "frame.%06d.ppm" (/ *image-count* 100))))
	  (with-current-buffer (create-file-buffer name)
	    (buffer-disable-undo)
	    (fundamental-mode)
	    (erase-buffer)
	    (let ((rows (length map))
		  (cols (length (aref map 0))))
	      (insert (day10-ppm-header cols rows))
	      (dotimes (row (length map))
		(dotimes (col (length (aref map 0)))
		  (insert (day10-color (day10-get map row col))))))
	    (write-file name)
	    (kill-buffer)))))))

(defun day10-find-edge (map key)
  ;;(day10-display map)
  (let ((path (list key))
	(seen (make-hash-table :test 'equal))
	(outside nil))
    (while path
      (let* ((p (pop path))
	     (row (car p))
	     (col (cdr p)))
	(let ((c (day10-get map row col)))
	  (cond ((gethash p seen))
		((= c ?B) (setf outside t))  ; hit a wall
		((= c ?P))		     ; hit a pipe
		((= c ?e) (setf outside t))  ; previously seen outside space
		((= c ?E) (setf outside t))  ; previously seen outside piece
		((= c ?i))		     ; previously seen inside
		(t (push (cons (1- row) col) path)
		   (push (cons (1+ row) col) path)
		   (push (cons row (1+ col)) path)
		   (push (cons row (1- col)) path)))
	  (setf (gethash p seen) c))))
    (maphash (lambda (k v)
	       (unless (or (= v ?B) (= v ?P))
		 (day10-write map)
		 (day10-set map (car k) (cdr k) (day10-outside-mark v outside))
		 ;;(day10-display map (car k) (cdr k))
		 ))
	     seen)))

(defun day10-outside-mark (char outsidep)
  (if outsidep
      (if (= char ?x) ?E ?e)
    ?i))
			   
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
	    (dotimes (r 3)
	      (dotimes (c 3)
		(day10-set big (+ r (* 3 row)) (+ c (* 3 col)) ?s)
		(day10-write big)))
	    (cl-incf score)))))))

(defun day10-get-path ()
  (beginning-of-buffer)
  (let ((pos (1- (search-forward "S"))))
    (goto-char pos)
    (day10-move 'north)			;shortcut, I konw there's a 7 to the north
    (day10-follow-path pos)))

(defun day10-convert-to-yx (pos)
  (cons (/ pos *line-length*) (1- (mod pos *line-length*))))

(defun day10-find-start ()
  (goto-char (point-min))
  (let ((pos (1- (search-forward "S"))))
    (day10-convert-to-yx pos)))

(defun day10-xpm2-header (height width)
  ;; doesn't work
  (format "! XPM2
%d %d 5 1
B c #000000
P c #0000ff
e c #880088
i c #888800
s c #ffffff
" width height))

(defun day10-ppm-header (height width)
  (format "P3
%d %d
7\n" width height))

(defun day10-part2 (buffer-name)
  (with-current-buffer buffer-name
    (goto-char (point-min))
    (let ((*line-length* (line-end-position)))
      (let* ((map (seq-into (aoc-buffer-lines buffer-name) 'vector))
	     (path (mapcar 'day10-convert-to-yx (day10-get-path)))
	     (*image-count* 0)
	     (big (day10-expand map path)))
	(let ((rc (day10-find-start)))
	  (cl-assert (= ?S (day10-get map (car rc) (cdr rc))) t))
	;;(day10-display big)
	(dotimes (r (length big))
	  (dotimes (c (length (aref big 0)))
	    (when (= ?x (day10-get big r c))
	      (day10-find-edge big (cons r c)))))
	(prog1 (day10-score map big)
	  (day10-write big))))))

(defun view-day10 ()
  (interactive)
  (switch-to-buffer (get-buffer-create "day10.output.ppm"))
  (day10-part2 "day10.2023.input.txt"))

;; (day10-part2 "day10.2023.input.txt")
