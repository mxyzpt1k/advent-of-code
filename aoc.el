;;; aoc.el - advent of code utilities
;;; started on Thursday, November 16, 2023

(defmacro aoc-copy-output (tag body)
  (declare (indent defun))
  (let ((var (gensym)))
    `(let ((,var (catch ,tag ,body)))
       (if (numberp ,var)
	   (setq ,var (number-to-string ,var)))
       (kill-new ,var)
       ,var)))

(defmacro aoc-code-block (body)
  ;; xxx - unused
  (declare (indent defun))
  `(aoc-copy-ooutput 'aoc-return ,body))

(defun aoc-return (v)
  ;; xxx -unused
  (throw 'aoc-return v))

(defun aoc-match-groups (n line)
  "return all match groups as strings"
  (let ((acc ()))
    (dotimes (k n acc)
      (push (match-string (1+ k) line) acc))
    (reverse acc)))

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

(defun aoc-uniq (list)
  (cond ((null list) ())
	((member (car list) (cdr list)) (aoc-uniq (cdr list)))
	(t (cons (car list) (aoc-uniq (cdr list))))))

(cl-defstruct aoc-grid x y grid x-size y-size)

(cl-defmethod aoc-grid-set ((grid aoc-grid) x y v)
  (let ((row (aref (aoc-grid-grid grid) y)))
    (setf (aref row x) v)))

(cl-defmethod aoc-grid-get ((grid aoc-grid) x y)
  (let ((row (aref (aoc-grid-grid grid) y)))
    (aref row x)))

(cl-defmethod aoc-walk-grid ((grid aoc-grid) fun &optional start end)
  (unless start
    (setq start (list 0 0)))
  (unless end
    (setq end (list (1- (aoc-grid-x-size grid)) (1- (aoc-grid-y-size grid)))))
  (cl-loop for y from (cadr start) upto (cadr end)
	   do (cl-loop for x from (car start)  upto (car end)
		       do (funcall fun grid x y))))

(defun aoc-make-grid (rows cols)
  (let ((grid (make-vector rows nil)))
    (dotimes (i cols)
      (setf (aref grid i) (make-vector cols 0)))
    (make-aoc-grid :x 0 :y 0 :grid grid :x-size cols :y-size rows)))

