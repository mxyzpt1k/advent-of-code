;;; Day 10: Pipe Maze
;;; Advent of Code 2023
;;; Sunday, December 10, 2023

(defun day10-move (direction)
  (cl-case direction
    (east (forward-char))
    (west (backward-char))
    (otherwise (let ((x (- (point) (line-beginning-position))))
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
