;;; day1.el - advent of code 2015
;;; started on Friday, November 17, 2023
;;; warming up for 2023

(defun day1-part1 (chars)
  (let ((floor 0))
    (dolist (c chars floor)
      (cond ((char-equal ?\( c) (cl-incf floor))
	    ((char-equal ?\) c) (cl-decf floor))
	    ))))

(let ((b (get-buffer "aoc-day1-2015.txt")))
  (day1-part1 (aoc-buffer-chars b)))

(defun day1-part2 (chars)
  (let ((floor 0)
	(idx 0))
    (dolist (c chars floor)
      (cl-incf idx)
      (cond ((char-equal ?\( c) (cl-incf floor))
	    ((char-equal ?\) c) (cl-decf floor))
	    )
      (if (< floor 0)
	  (throw 'done idx)))))

(let ((b (get-buffer "aoc-day1-2015.txt")))
  (catch 'done 
    (day1-part2 (aoc-buffer-chars b))))

