;;; aoc.el - advent of code utilities
;;; started on Thursday, November 16, 2023

(defmacro aoc-copy-output (tag body)
  (declare (indent defun))
  (let ((var (gensym)))
    `(let ((,var (catch ,tag ,body)))
       (when (numberp ,var)
	 (setq ,var (number-to-string ,var)))
       (kill-new ,var)
       ,var)))

(defun aoc-add-border-command ()
  "add a character to each end of every line and the beginning and end of the buffer"
  (interactive)
  (let ((border-char (read-from-minibuffer "border char? ")))
    (save-excursion
      (beginning-of-buffer)
      (replace-regexp "^" border-char)
      (beginning-of-buffer)
      (replace-regexp "$" border-char)
      (beginning-of-buffer)
      (let ((len (1- (line-end-position))))
	(insert (make-string len (aref border-char 0)) "\n")
	(end-of-buffer)
	(insert "\n" (make-string len (aref border-char 0)) "\n")))))

(defun aoc-add-border (grid &optional char)
  (unless char
    (setq char ?#))
  (let ((cols (length (aref grid 0)))
	(rows (length grid)))
    (let ((new (make-vector (+ 2 rows) ())))
      (setf (aref new 0) (make-vector (+ 2 cols) char))
      (setf (aref new (1+ rows)) (make-vector (+ 2 cols) char))
      (dotimes (r rows)
	(setf (aref new (1+ r)) (vconcat (vector char) (aref grid r) (vector char))))
      new)))

(defun aoc-match-groups (n line)
  "return all match groups as strings"
  (let ((acc ()))
    (dotimes (k n (reverse acc))
      (push (match-string (1+ k) line) acc))))

(defun aoc-do-permutations (fun list &optional acc)
  (if (null list)
      (funcall fun acc)
    (dolist (elt list)
      (let ((new (remove elt list)))
	(aoc-do-permutations fun new (cons elt acc))))))

(defun aoc-buffer-lines (buf)
  "return the lines in buffer BUF as a list"
  (with-current-buffer buf
    (string-lines (buffer-string))))

(defun aoc-buffer-chars (buf)
  "return the contents of buffer BUF as a list of chars"
  (with-current-buffer buf
    (string-to-list (buffer-string))))
    
(defun aoc-split-list (tok list)
  "split LIST on TOK and return a list of lists"
  (let ((acc ())
	(cur ()))
    (dolist (elm list)
      (cond ((equal elm tok)
	     (push (reverse cur) acc)
	     (setq cur ()))
	    (t (push elm cur))))
    (reverse acc)))

(defun aoc-sum (list)
  (seq-reduce #'+ list 0))

;; use #'seq-uniq 
;; (defun aoc-uniq (list)
;;   (cond ((null list) ())
;; 	((member (car list) (cdr list)) (aoc-uniq (cdr list)))
;; 	(t (cons (car list) (aoc-uniq (cdr list))))))

(cl-defstruct aoc-grid x y grid x-size y-size)

(cl-defmethod aoc-grid-set ((grid aoc-grid) x y v)
  (when (and (< -1 x (aoc-grid-x-size grid))
	     (< -1 y (aoc-grid-y-size grid)))
    (let ((row (aref (aoc-grid-grid grid) y)))
      (setf (aref row x) v))))

(cl-defmethod aoc-grid-get ((grid aoc-grid) x y &optional v)
  (if (and (< -1 x (aoc-grid-x-size grid))
	     (< -1 y (aoc-grid-y-size grid)))
      (let ((row (aref (aoc-grid-grid grid) y)))
	(aref row x))
    v))

(cl-defmethod aoc-grid-get-default ((grid aoc-grid) x y default)
  "get (x y) from grid, if x or y is out of bounds, return default"
  ;; keeping this in case something in 2015 uses it
  (aoc-grid-get grid x y default))

(cl-defmethod aoc-walk-grid ((grid aoc-grid) fun &optional start end)
  (unless start
    (setq start (list 0 0)))
  (unless end
    (setq end (list (1- (aoc-grid-x-size grid)) (1- (aoc-grid-y-size grid)))))
  (cl-loop for y from (cadr start) upto (cadr end)
	   do (cl-loop for x from (car start)  upto (car end)
		       do (funcall fun grid x y))))

(cl-defmethod aoc-grid-copy ((grid aoc-grid))
  (let ((rows (make-vector (aoc-grid-y-size grid) nil)))
    (seq-map-indexed (lambda (elt idx)
		       (setf (aref rows idx) (seq-copy elt)))
		     (aoc-grid-grid grid))
    (make-aoc-grid :grid rows :y-size (length rows) :x-size (length (aref rows 0)))))

(defun aoc-make-grid (rows cols &optional elt)
  (let ((grid (make-vector rows nil)))
    (dotimes (i rows)
      (aset grid i (make-vector cols elt)))
    (make-aoc-grid :x 0 :y 0 :grid grid :x-size cols :y-size rows)))

