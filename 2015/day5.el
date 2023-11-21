;;; day5.el - advent of code 2015
;;; Saturday, November 18, 2023
;;; warming up for this year

(defun nice-p (word)
  "part 1"
  (and (string-match "[aeiou].*[aeiou].*[aeiou]" word)
       (string-match "\\(.\\)\\1" word)
       (not (string-match "ab\\|cd\\|pq\\|xy" word))))

(defun nice-p (word)
  "part 2"
  (and (string-match "\\(..\\).*\\1" word)
       (string-match "\\(.\\).\\1" word)))

(aoc-copy-output 'done
 (let ((nice 0))
   (dolist (word (aoc-buffer-lines "day5.input.txt") nice)
     (when (nice-p word)
       (cl-incf nice)))))

(progn
  (cl-assert (nice-p "ugknbfddgicrmopn"))
  (cl-assert (nice-p "aaa"))
  (cl-assert (not (nice-p "jchzalrnumimnmhp")))
  (cl-assert (not (nice-p "haegwjzuvuyypxyu")))
  (cl-assert (not (nice-p "dvszwmarrgswjxmb"))))

(progn
  (cl-assert (nice-p "qjhvhtzxzqqjkmpb"))
  (cl-assert (nice-p "xxyxx"))
  (cl-assert (not (nice-p "uurcxstgmygtbstg")))
  (cl-assert (not (nice-p "ieodomkazucvgmuy"))))
