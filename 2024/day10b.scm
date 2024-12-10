;;; Advent of Code
;;; Tuesday, December 10, 2024

;;; removed some unnecessary code

(define (count-trails point grid)
  (let ((seen (aoc-make-grid (aoc-grid-rows grid) (aoc-grid-cols grid) #f))
	(count 0))
    (define (altitude r c) (aoc-grid-ref grid r c))
    (define (make-key r c) (+ c (* r (aoc-grid-rows grid))))
    (define (seen? r c) (aoc-grid-ref seen r c))
    (define (helper r c)
      (cond ((seen? r c) 0)
	    (else (aoc-grid-set! seen r c #t) ;comment this out to solve part 2
		  (cond ((= 9 (altitude r c)) (set! count (+ 1 count)))
			(else (if (= (+ 1 (altitude r c)) (aoc-grid-ref grid (+ 1 r) c))
				  (helper (+ 1 r) c))
			      (if (= (+ 1 (altitude r c)) (aoc-grid-ref grid (+ -1 r) c))
				  (helper (+ -1 r) c))
			      (if (= (+ 1 (altitude r c)) (aoc-grid-ref grid r (+ 1 c)))
				  (helper r (+ 1 c)))
			      (if (= (+ 1 (altitude r c)) (aoc-grid-ref grid r (+ -1 c)))
				  (helper r (+ -1 c))))))))
    (helper (car point) (cdr point))
    count))
				
(define (day10-part1 grid)
  (let ((grid (aoc-expand-grid grid 99))
	(sum 0))
    (aoc-walk-grid grid
      (lambda (g r c)
	(if (zero? (aoc-grid-ref g r c))
	    (set! sum (+ sum (count-trails (cons r c) grid))))))
    sum))

(define (day10-start file solve)
  (let ((grid (aoc-read-grid file)))
    (aoc-walk-grid grid
      (lambda (g r c)
	(aoc-grid-set! g r c (aoc-char-value (aoc-grid-ref g r c)))))
    (solve grid)))

;; (day10-start "day10.2024.input.txt" day10-part1)
;; (day10-start "day10.test" day10-part1)
