;;; Advent of Code
;;; Tuesday, December 10, 2024

;;       --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;;  10   01:23:51   9252      0   01:29:13   8830     0 

;; (load "aoc.scm")
;; using scheme48 hash-tables
;; ,open tables

(define (make-topo-map grid)
  (let ((ht (make-integer-table)))
    (aoc-walk-grid grid
      (lambda (g r c)
	(let ((v (aoc-grid-ref g r c)))
	  ;; + 1 because we know the map will be expanded
	  (table-set! ht v (cons (cons (+ 1 r) (+ 1 c)) (or (table-ref ht v) '()))))))
    ht))

(define (count-trails point grid topo)
  (let ((seen (make-integer-table))
	(count 0))
    (define (altitude r c) (aoc-grid-ref grid r c))
    (define (make-key r c) (+ c (* r (aoc-grid-rows grid))))
    (define (seen? r c) (table-ref seen (make-key r c)))
    (define (helper r c)
      (cond ((seen? r c) 0)
	    (else (table-set! seen (make-key r c) #t) ;comment this out to solve part 2
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
    ;;(display count) (display ", ")
    count))
				
(define (day10-part1 grid)
  (let* ((topo (make-topo-map grid))
	 (grid (aoc-expand-grid grid 99)))
    (define (helper heads sum)
      (if (null? heads)
	  sum
	  (helper (cdr heads) (+ sum (count-trails (car heads) grid topo)))))
    (helper (table-ref topo 0) 0)))

(define (day10-start file solve)
  (let ((grid (aoc-read-grid file)))
    (aoc-walk-grid grid
      (lambda (g r c)
	(aoc-grid-set! g r c (aoc-char-value (aoc-grid-ref g r c)))))
    (solve grid)))

;; (day10-start "day10.2024.input.txt" day10-part1)
;; (day10-start "day10.test" day10-part1)

	       
	       
