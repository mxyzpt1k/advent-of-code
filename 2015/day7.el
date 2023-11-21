;;; day7.el - advent of code 2015
;;; Saturday, November 18, 2023
;;; warming up for 2023

(defun fix-number (s)
  (if (string-match "^[0-9]+$" s)
      (string-to-number s)
    s))

(defun match-groups (n line)
  (let ((acc ()))
    (dotimes (k n acc)
      (push (match-string (1+ k) line) acc))
    (mapcar #'fix-number (reverse acc))))

(defun compile-wire (line wires)
  (cond ((string-match "\\(\\w+\\) \\([A-Z]+\\) \\(\\w+\\) -> \\([a-z]+\\)" line)
	 (cl-destructuring-bind (lhs op rhs dest) (match-groups 4 line)
	   (setf (gethash dest wires) (list op lhs rhs))))
	((string-match "NOT \\(\\w+\\) -> \\([a-z]+\\)" line)
	 (cl-destructuring-bind (src dest) (match-groups 2 line)
	   (setf (gethash dest wires) (list "NOT" src))))
	((string-match "\\(\\w+\\) -> \\(\\w+\\)" line)
	 (cl-destructuring-bind (src dest) (match-groups 2 line)
	   (setf (gethash dest wires) src)))
	(t (throw 'what "unexpected phrase"))))

(defun right-shift (a b)
  (if (> a 65536)
      (throw 'what "too big")
    (ash a (- b))))

(defun eval-wire (var env)
  (if (numberp var)
      var
    (let ((val (gethash var env)))
      (let ((new-val
	     (cond ((numberp val) val)
		   ((stringp val) (eval-wire val env))
		   ((equal "RSHIFT" (car val))
		    (apply #'right-shift (mapcar (lambda (s) (eval-wire s env)) (cdr val))))
		   ((equal "LSHIFT" (car val))
		    (apply #'ash (mapcar (lambda (s) (eval-wire s env)) (cdr val))))
		   ((equal "AND" (car val))
		    (apply #'logand (mapcar (lambda (s) (eval-wire s env)) (cdr val))))
		   ((equal "OR" (car val))
		    (apply #'logior (mapcar (lambda (s) (eval-wire s env)) (cdr val))))
		   ((equal "NOT" (car val))
		    (lognot (eval-wire (cadr val) env)))
		   (t (throw 'what (format "bad eval: %s <- %s" var val))))))
	(setf (gethash var env) new-val)))))
      
(aoc-copy-output 'what
  (let ((wires (make-hash-table :test 'equal)))
    (dolist (line (aoc-buffer-lines "day7.input.txt"))
      (compile-wire line wires))
    (setf (gethash "b" wires) 46065)	;part b
    (eval-wire "a" wires)))

