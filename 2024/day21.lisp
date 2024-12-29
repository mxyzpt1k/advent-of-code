;;; Advent of Code
;;; Sunday, December 22, 2024 (but working on day 21)

(defclass keypad ()
  ((row :initform 0 :accessor keypad-row :initarg :row)
   (col :initform 0 :accessor keypad-col :initarg :col)
   (score :initform 0 :accessor keypad-score)
   (grid :initform nil :accessor keypad-grid)))

(defclass numpad (keypad)
  ((row :initform 3)
   (col :initform 2)
   (grid :initform #2A((7 8 9)
		       (4 5 6)
		       (1 2 3)
		       (() 0 :A)))))

(defclass dirpad (keypad)
  ((row :initform 0)
   (col :initform 2)
   (grid :initform #2A((() :UP :A)
		       (:LEFT :DOWN :RIGHT)))))

(defmethod pad-ref ((grid array) row col)
  (destructuring-bind (rs cs) (array-dimensions grid)
    (cond ((or (< row 0) (< col 0)) nil)
	  ((or (<= rs row) (<= cs col)) nil)
	  (t (aref grid row col)))))

(defmethod pad-ref ((kp keypad) row col)
  (pad-ref (keypad-grid kp) row col))

(defun opposite (move)
  (case move
    ((:left) :right)
    ((:right) :left)
    ((:up) :down)
    ((:down) :up)))

(defun find-path (grid r c target seen callback &optional moves)
  (let ((num (pad-ref grid r c)))
    (cond ((null num) nil)
	  ((member num seen) nil)
	  ((member (opposite (car moves)) (cdr moves)) nil)
	  ((eql target num) (funcall callback (reverse (cons :A moves))))
	  (t (let ((seen (cons num seen)))
	       (find-path grid r (1- c) target seen callback (cons :left moves))
	       (find-path grid r (1+ c) target seen callback (cons :right moves))
	       (find-path grid (1- r) c target seen callback (cons :up moves))
	       (find-path grid (1+ r) c target seen callback (cons :down moves)))))))

(defmethod update! ((kp keypad) key)
  (let ((grid (keypad-grid kp)))
    (destructuring-bind (rows cols) (array-dimensions grid)
      (dotimes (r rows)
	(dotimes (c cols)
	  (when (eql key (aref grid r c))
	    (setf (keypad-row kp) r)
	    (setf (keypad-col kp) c)))))))

(defmethod move-to ((kp keypad) target callback)
  (let ((grid (keypad-grid kp))
	(row (keypad-row kp))
	(col (keypad-col kp)))
    (find-path grid row col target () callback)))

(defmethod collect ((kp keypad) moves)
  (let ((acc ()))
    (dolist (m moves (reverse acc))
      (let ((choices ()))
	(move-to kp m (lambda (list) (push list choices)))
	(update! kp m)
	(push choices acc)))))

(defun combine (lists &optional acc)
  (cond ((null lists) acc)
	((null acc) (combine (cdr lists) (car lists)))
	(t (let ((aacc ()))
	     (dolist (c (car lists) (combine (cdr lists) aacc))
	       (dolist (a acc)
		 (push (append a c) aacc)))))))

(defun shortest (lols)
  (let ((best (length (car lols)))
	(acc ()))
    (dolist (l lols acc)
      (let ((len (length l)))
	(cond ((= len best) (push l acc))
	      ((< len best) (setf acc () best len acc (list l))))))))

(defmethod dir-moves ((dp dirpad) moves)
  (let ((acc ())
	(row (keypad-row dp))
	(col (keypad-col dp)))
    (dolist (m moves (shortest acc))
      (setf (keypad-row dp) row (keypad-col dp) col)
      (setf acc (append (combine (collect dp m)) acc))
      )))

(defmethod num-moves (list)
  (let ((np (make-instance 'numpad))
	(dpad1 (make-instance 'dirpad))
	(dpad2 (make-instance 'dirpad))
	(sum 0))
    (dolist (n list sum)
      (let ((acc ()))
	(move-to np n (lambda (list) (push list acc)))
	(update! np n)
	;;(print acc)
	(let ((moves (dir-moves dpad2 (dir-moves dpad1 acc))))
	  (incf sum (reduce #'min (mapcar #'length moves))))))))

(defun part1-test ()
  (+ (* 29 (num-moves '(0 2 9 :A)))
     (* 980 (num-moves '(9 8 0 :A)))
     (* 179 (num-moves '(1 7 9 :A)))
     (* 456 (num-moves '(4 5 6 :A)))
     (* 379 (num-moves '(3 7 9 :A)))))
;;(part1-test)
	      
(defun part1 ()
  (+ (* 480 (num-moves '(4 8 0 :A)))
     (* 143 (num-moves '(1 4 3 :A)))
     (* 983 (num-moves '(9 8 3 :A)))
     (* 382 (num-moves '(3 8 2 :A)))
     (* 974 (num-moves '(9 7 4 :A)))))
;;(part1)
