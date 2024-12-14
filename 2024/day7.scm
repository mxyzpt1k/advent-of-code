;;; Saturday, December 07, 2024
;;; Advent of Code

;;; Modify the input data to make it easier to read.
;;; sed 's/://; s/^/(/; s/$/)/' ~/Downloads/day7.2024.input.txt
;;; "190: 10 19" => "(190 10 19)"

;;; part 2 takes just under a second in petite chez scheme on an M1 mac

(define (possible? ops target value nums)
  (cond ((< target value) #f)
	((null? nums) (= target value))
	(else
	 (let loop ((fs ops))
	   (cond ((null? fs) #f)
		 ((possible? ops target ((car fs) value (car nums)) (cdr nums)) #t)
		 (else (loop (cdr fs))))))))

(define (day7-part1 numbers)
  (possible? (list + *) (car numbers) (cadr numbers) (cddr numbers)))

(define (day7-part2 numbers)
  (define (concat a b)
    (let ((p (inexact->exact (floor (+ 1 (/ (log b) (log 10)))))))
      (+ b (* (expt 10 p) a))))
  (possible? (list + * concat) (car numbers) (cadr numbers) (cddr numbers)))

(define (solve part)
  (let loop ((nums (read))
	     (sum 0))
    (cond ((eof-object? nums) sum)
	  ((part nums) (loop (read) (+ sum (car nums))))
	  (else (loop (read) sum)))))

;;(time (with-input-from-file "input.7" (lambda () (solve day7-part2))))

