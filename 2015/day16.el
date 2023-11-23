;;; day16.el - advent of code 2015
;;; Wednesday, November 22, 2023, 11:40
;;; warming up for 2023

(defun day16 ()
  (let ((found 0))
    (dolist (line (aoc-buffer-lines "day16.input.txt") found)
      (let ((rec (string-split line)))
	(cl-assert (= 8 (length rec)))
	(cl-destructuring-bind (sue num s1 n1 s2 n2 s3 n3) rec
	  (when (and (match-sue-p s1 (string-to-number n1))
		     (match-sue-p s2 (string-to-number n2))
		     (match-sue-p s3 (string-to-number n3)))
	    (setq found (string-replace ":" "" num))))))))

(defun match-sue-p (what amount)
  (let ((wanted `(("children:" 3  ,#'=)
		  ("cats:"  7     ,#'>)
		  ("samoyeds:" 2  ,#'= a b)
		  ("pomeranians:" 3  ,#'< a b)
		  ("akitas:" 0    ,#'=)
		  ("vizslas:"  0  ,#'=)
		  ("goldfish:" 5  ,#'<)
		  ("trees:" 3     ,#'>)
		  ("cars:" 2      ,#'=)
		  ("perfumes:" 1  ,#'=))))
    (let ((kv (assoc what wanted)))
      (and kv (funcall (caddr kv)  amount (cadr kv))))))

(aoc-copy-output () (day16))

