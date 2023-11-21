;;; day12.el - advent of code 2015
;;; Monday, November 20, 2023
;;; warming up for this year

(require 'json)

(defun contains-red? (alist)
  (let ((red nil))
    (dolist (pair alist red)
      (if (equal "red" (cdr pair))
	  (setq red t)))))
    
(defun json-sum (js)
  (cond ((numberp js) js)
	((symbolp js) 0)
	((stringp js) 0)
	((arrayp js) (aoc-sum (seq-map #'json-sum js)))
	((contains-red? js) 0)
	(t (aoc-sum (mapcar (lambda (elt) (json-sum (cdr elt))) js)))))

(let ((test "[{\"a\":[1,2,3],\"b\":4}]"))
  (let ((js (json-read-from-string test)))
    (json-sum js)))

(aoc-copy-output ()
  (let ((sum 0))
    (dolist (line (aoc-buffer-lines "day12.input.txt") sum)
      ;;(insert (format "\n line length = %d" (length line)))
      (setq sum (json-sum  (json-read-from-string line))))))

 line length = 37047

;; 133004 too low

