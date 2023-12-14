;;; Day 14: Parabolic Reflector Dish
;;; Advent of Code 2023
;;; Thursday, December 14, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  14   00:53:10   6480      0   09:32:18  14102      0

;;; inefficient, but I went to bed and solved it in the morning
;;; using the output buffer. there was enough data at around the
;;; first hour, but I needed the sleep anyway.

;;; ((1B - start-of-cycle) mod size-of-cycle) = cycle-offset

;;; part 1

(defun day14-get (board r c)
  (aref (aref board r) c))

(defun day14-set (board r c thing)
  (let ((row (aref board r)))
    (setf (aref row c) thing)))
	   
(defun day14-swap (board ra ca rb cb)
  (let ((temp (day14-get board ra ca)))
    (day14-set board ra ca (day14-get board rb cb))
    (day14-set board rb cb temp)))

(defun roll-rocks-north (board)
  (let ((changes t))
    (while changes
      (setf changes nil)
      (dotimes (r (1- (length board)))
	(dotimes (c (1- (length (aref board 0))))
	  (when (and (= ?O (day14-get board (1+ r) c))
		     (= ?. (day14-get board r c)))
	    (day14-swap board (1+ r) c r c)
	    (setf changes t)))))))

(defun score-rocks (board)
  (let ((weight 0)
	(score 0))
    (dotimes (r (length board) score)
      (dotimes (c (length (aref board r)))
	(when (= ?O (day14-get board r c))
	  (cl-incf score r))))))

(defun display-rocks (board)
  (seq-do (lambda (row) (insert "\n" (seq-into row 'string)))
	  board))
  
(defun day14-part-1 (buffer-name)
  (let ((acc ()))
    (dolist (line (aoc-buffer-lines buffer-name))
      (push (vconcat line) acc))
    (let ((board (vconcat (reverse acc))))
      (roll-rocks-north board)
      ;;(display-rocks board)
      (score-rocks (reverse board)))))
      
;; (aoc-copy-output () (day14-part-1 "day14.2023.input.txt"))
;; (day14-part-1 "test.buff")

;;; part 2

(defun roll-rocks (direction board)
  (let ((changes t))
    (while changes
      (setf changes nil)
      (dotimes (r (1- (length board)))
	(dotimes (c (1- (length (aref board 0))))
	  (seq-let (y x) (funcall direction r c)
	    (when (and (= ?. (day14-get board r c))
		       (= ?O (day14-get board y x)))
	      (day14-swap board y x r c)
	      (setf changes t))))))))

(defun to-north (r c) (list (1+ r) c))

(defun to-south (r c) (list (1- r) c))

(defun to-east (r c) (list r (1- c)))

(defun to-west (r c) (list r (1+ c)))

(defun cycle-rocks (board)
  ;; one cycle of the board rolls
  (roll-rocks 'to-north board)
  (roll-rocks 'to-west board)
  (roll-rocks 'to-south board)
  (roll-rocks 'to-east board)
  (score-rocks (reverse board)))

(defun test-cycles (board times)
  (let ((seen (make-hash-table)))
    (dotimes (k times)
      (message "cycle %d" (1+ k))
      (let ((score (cycle-rocks board)))
	(with-current-buffer "day14.test.out"
	  (insert (format "\n%d. %d   %d" k score (- k (gethash score seen 0))))
	  (setf (gethash score seen) k))))))
    
(defun day14-part-2 (buffer-name)
  (let ((acc ())
	(*output-score* t))
    (dolist (line (aoc-buffer-lines buffer-name))
      (push (vconcat line) acc))
    (let ((board (vconcat (reverse acc))))
      (test-cycles board 500)
      ;;(roll-rocks 'to-south board)
      ;;(display-rocks board)
      )))

;; (day14-part-2 "test.buff")
;; (day14-part-2 "day14.2023.input.txt")
