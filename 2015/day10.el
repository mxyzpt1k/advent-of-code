;;; day10.el - advent of code 2015
;;; Sunday, November 19, 2023
;;; warming up for 2023

(defun expander (s)
  (format "%d%c" (length s) (seq-elt s 0)))

(defun look-and-say (s)
  (replace-regexp-in-string "\\([[:digit:]]\\)\\1*" #'expander s))

Ba = 11131

;; part 1 = 360154
(aoc-copy-output ()
  (let* ((seed "1113122113")
	 (prev (length seed)))
    (dotimes (i 10 (length seed))
      (setq seed (look-and-say seed))
      (insert (format "\n%s" seed))
      (setq prev (length seed))
      )))

;; part 2 = 5103798
(aoc-copy-output ()
  (let ((seed (list "Fr"))) 		;Fr is "1113122113"
    (dotimes (n 50 (look-and-say-length seed))
      (setq seed (decay-element seed)))))

;; 5103798 - see look-and-say.el
;; 5712667 is too high
;; 5103357 is too low
;; 4961469 is too low
