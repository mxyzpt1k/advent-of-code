;;; Day 8: Haunted Wasteland
;;; Advent of Code 2023
;;; Friday, December 08, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   8   00:42:56   9735      0   01:13:58   5920      0

(require 'peg)

(defun day8-read-rule ()
  (with-peg-rules
      ((rule (bol) (substring word) " = (" (substring word) ", " (substring word) ")"
	     `(a b c -- c b a))
       (word (+ char))
       (char (or (range ?0 ?9) (range ?A ?Z))))
    (peg-run (peg rule))))

(defun day8-part-1 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((L/R (string-trim (thing-at-point 'line t)))
	  (rules (make-hash-table :test 'equal)))
      (forward-line 2)
      (cl-do ((rule #1=(day8-read-rule) #1#))
	  ((null rule) t)
	(setf (gethash (car rule) rules) (cdr rule))
	(setq rule (day8-read-rule))
	(forward-line))
      (let ((count 0)
	    (location "AAA"))
	(while t
	  (seq-do (lambda (lr)
		    (let ((pair (gethash location rules)))
		      (cl-incf count)
		      (setq location (cond ((= ?L lr) (car pair))
					   ((= ?R lr) (cadr pair))
					   (t (throw 'huh lr))))
		      (when (string-equal "ZZZ" location)
			(throw 'done count))))
		  L/R))))))

(defun day8-part-2 (buffer-name)
  (with-current-buffer buffer-name
    (beginning-of-buffer)
    (let ((L/R (string-trim (thing-at-point 'line t)))
	  (rules (make-hash-table :test 'equal)))
      (forward-line 2)
      (cl-do ((rule #1=(day8-read-rule) #1#))
	  ((null rule) t)
	(setf (gethash (car rule) rules) (cdr rule))
	(forward-line))
      (let ((counts ())
	    (locations ()))
	(maphash (lambda (k v) (when (= ?A (aref k 2)) (push k locations))) rules)
	(dolist (loc locations (apply 'cl-lcm counts))
	  (push (catch 'done (day8-navigate loc)) counts))))))

(defun day8-navigate (location)
  (let ((count 0))
    (while t
      (seq-do (lambda (lr)
		(let ((pair (gethash location rules)))
		  (cl-incf count)
		  (setq location (cond ((= ?L lr) (car pair))
				       ((= ?R lr) (cadr pair))
				       (t (throw 'huh lr))))
		  (when (= ?Z (aref location 2))
		    (throw 'done count))))
	      L/R))))

;; (aoc-copy-output 'done (day8-part-2 "test.buff"))
;; (benchmark-run (aoc-copy-output 'done (day8-part-2 "day8.2023.input.txt")))
;;  => 0.395 seconds
