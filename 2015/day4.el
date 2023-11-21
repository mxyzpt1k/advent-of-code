;;; day4.el - advent of code 2015
;;; Friday, November 17, 2023

(aoc-copy-output 'done
 (let ((key "ckczppom")
       (num -1))
   (while t
     (cl-incf num)
     ;(if (zerop (mod num 100000)) (message "%d" num))
     (if (> num 4000000)
	 (throw 'done 0))
     (let ((h (md5 (concat key (number-to-string num)))))
       (if (equal "000000" (substring h 0 6))
	   (throw 'done num))))))

(md5 (concat "ckczppom" "3938038"))



