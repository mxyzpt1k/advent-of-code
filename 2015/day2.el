;;; day2.el - advent of code 2015
;;; Friday, November 17, 2023

(defun paper-size (line)
  (cl-destructuring-bind (h w l) (mapcar #'string-to-number (string-split line "x" t))
    (let ((hw (* h w))
	  (hl (* h l))
	  (wl (* w l)))
      (+ (min hw hl wl)
	 hw hw
	 hl hl
	 wl wl))))

(defun ribbon-size (line)
  (cl-destructuring-bind (h w l) (mapcar #'string-to-number (string-split line "x" t))
    (let ((a (+ h h w w))
	  (b (+ h h l l))
	  (c (+ l l w w)))
      (let ((len (min a b c)))
	(+ len (* h w l))))))

(aoc-copy-output
 (let ((total 0))
   (dolist (line (aoc-buffer-lines "day2.2015.txt") total)
     (cl-incf total (ribbon-size line)))))




    
