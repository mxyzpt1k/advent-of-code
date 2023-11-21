;;; day13.el - advent of code 2015
;;; Monday, November 20, 2023
;;; warming up for this year

(defun score-happeness (a b env)
  (if (or (string= a "Dennis") (string= b "Dennis"))
      0
    (let ((ab (assoc (cons a b) env))
	  (ba (assoc (cons b a) env)))
      (+ (cdr ab) (cdr ba)))))

(defun seating-score (perm env)
  (let ((first (car perm))
	(last (car perm))
	(score 0))
    (dolist (who (cdr perm))
      (cl-incf score (score-happeness last who env))
      (setq last who))
    (+ score (score-happeness last first env))))
  
(defun solve-happiness (names env)
  (let ((best 0))
    (aoc-do-permutations
     (lambda (perm)
       (let ((score (seating-score perm env)))
	 (when (> score best)
	   (setq best score)
	   (message "%s" score))))
     names)
    best))

(defun store-happiness (subj move points obj env)
  (let ((sign (if (equal move "gain") "" "-")))
    (let ((num (string-to-number (concat sign points))))
      (cons (cons (cons subj obj) num) env))))

(defun test-happiness-env (env)
  (cl-assert (= 56 (length env))))

(aoc-copy-output ()
  (let ((happiness 0)
	(env ())
	(names (list "Dennis")))
    (dolist (line (aoc-buffer-lines "day13.input.txt"))
      (let ((tokens (split-string (string-trim line " " "."))))
	(cl-destructuring-bind
	    (subj would move points happiness units by sitting next to obj)
	    tokens
	  (unless (member subj names)
	    (push subj names))
	  (setq env (store-happiness subj move points obj env)))))
    (solve-happiness names env)))
    
