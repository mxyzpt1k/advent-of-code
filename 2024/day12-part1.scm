;;; Advent of Code
;;; Thursday, December 12, 2024

;;(load "aoc.scm")

(define (price-region points)
  (define (exists? pred? p points)
    (cond ((null? points) #f)
	  ((pred? p (car points)) #t)
	  (else (exists? pred? p (cdr points)))))
  (define (north p q) (and (= (cdr p) (cdr q)) (= 1 (- (car p) (car q)))))
  (define (east p q) (and (= (car p) (car q)) (= 1 (- (cdr q) (cdr p)))))
  (define (south p q) (north q p))
  (define (west p q) (east q p))
  
  (define (shared-sides p)		;part 1
    (let ((sides 0))
      (if (exists? north p points) (set! sides (+ 1 sides)))
      (if (exists? south p points) (set! sides (+ 1 sides)))
      (if (exists? east p points) (set! sides (+ 1 sides)))
      (if (exists? west p points) (set! sides (+ 1 sides)))
      sides))

  (let* ((area (length points))
	 (shared (map shared-sides points))
	 (border (- (* 4 area) (aoc-reduce + shared 0))))
    (* area border)))

(define (map-region g r c region-id)
  (define (north r c)
    (and (< 0 r) (aoc-grid-ref g (+ -1 r) c)))
  (define (south r c)
    (and (< (+ 1 r) (aoc-grid-rows g)) (aoc-grid-ref g (+ 1 r) c)))
  (define (east r c)
    (and (< (+ 1 c) (aoc-grid-cols g)) (aoc-grid-ref g r (+ 1 c))))
  (define (west r c)
    (and (< 0 c) (aoc-grid-ref g r (+ -1 c))))

  (define (fill ch r c)
    (aoc-grid-set! g r c region-id)
    (if (eqv? ch (north r c)) (fill ch (+ -1 r) c))
    (if (eqv? ch (south r c)) (fill ch (+ 1 r) c))
    (if (eqv? ch (east r c)) (fill ch r (+ 1 c)))
    (if (eqv? ch (west r c)) (fill ch r (+ -1 c))))
  (fill (aoc-grid-ref g r c) r c))

(define (region-cost g r c id)
  (map-region g r c id)
  (let ((points '()))
    (aoc-walk-grid g
      (lambda (g r c)
	(if (eqv? id (aoc-grid-ref g r c))
	    (set! points (cons (cons r c) points)))))
    (price-region points)))

(define (part1 grid)
  (let ((cost 0)
	(region-id 0))
    (aoc-walk-grid grid
      (lambda (g r c)
	(if (char? (aoc-grid-ref g r c))
	    (begin
	      (set! region-id (+ 1 region-id))
	      (set! cost (+ cost (region-cost g r c region-id)))))))
    cost))

;; (part1 (aoc-read-grid "day12.2024.input.txt"))
