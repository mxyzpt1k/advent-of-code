;;; day8.el - advent of code 2015
;;; Sunday, November 19, 2023
;;; warming up for 2023

(defun mem-size (line)
  (let ((s line))
    (setq s (replace-regexp-in-string "\\\\\\\\" "/" s))
    (setq s (replace-regexp-in-string "\\\\x[a-f0-9][a-f0-9]" "." s))
    (setq s (replace-regexp-in-string "\\\\." "-" s))
    (setq s (replace-regexp-in-string "^\"" "" s))
    (setq s (replace-regexp-in-string "\"$" "" s))))

;; testing
;;(mapcar #'count-chars (aoc-buffer-lines "test.buffer"))
;;(seq-reduce #'+ (mapcar #'length (aoc-buffer-lines "test.buffer")) 0)
;;(seq-reduce #'+ (mapcar (lambda (s) (length (mem-size s))) (aoc-buffer-lines "test.buffer")) 0)

(defun count-chars (line)
  (let ((len 0)
	(pos 0)
	(state 'start))
    (while (and (< pos (length line)) (not (eql 'end state)))
      (let ((c (elt line pos)))
	(cl-case state
	  ('start (cl-assert (equal ?\" c))
		  (setq state 'read))
	  ('read (cond ((equal ?\\ c) (setq state 'escape))
		       ((equal ?\" c) (setq state 'end))
		       (t (cl-incf len))))
	  ('escape (when (string-match "x[[:xdigit:]][[:xdigit:]]" line pos)
		     (cl-incf pos 2))
		   (cl-incf len)
		   (setq state 'read))
	  (t (cl-assert nil "unexpected state")))
	(cl-incf pos)))
    len))

(aoc-copy-output ()
  (let ((code-len 0)
	(mem-size 0))
    (dolist (line (aoc-buffer-lines "day8.input.txt"))
      (cl-incf code-len (length line))
      (cl-incf mem-size (length (car (read-from-string line)))))
    (- code-len mem-size)))

;; 1345 - too high

;; $ cat ~/Downloads/day8.input.txt | perl -lpe 'chomp; $_ = eval $_;' | wc
;;     300     304    5160
;; $ wc ~/Downloads/day8.input.txt 
;;     300     300    6502 /Users/marti/Downloads/day8.input.txt
;; m1-mini:2015 marti$ bc -l
;; >>> 6502 - 5160
;; 1342

(cdr (read-from-string "egxjqytcttr\\ecfedmmovkyn\"m"))
(length "egxjqytcttr\\ecfedmmovkyn\"m")

(read-from-string "abc\x77")
(length "abc\x77")
    
