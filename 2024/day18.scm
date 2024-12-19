;;; Advent of Code
;;; Wednesday, December 18, 2024

(load "aoc.scm")

(define (mark grid score todo)
  
  (define (visit? row col done)
    (cond ((or (> 0 row) (> 0 col)) #f)
	  ((or (= (aoc-grid-rows grid) row) (= (aoc-grid-cols grid) col)) #f)
	  ((number? (aoc-grid-ref grid row col)) #f)
	  ((aoc-grid-ref done row col) #f)
	  (else (aoc-grid-set! done row col #t)
		(char=? #\. (aoc-grid-ref grid row col) ))))
  
  (define (neighbors pt)
    (let ((row (car pt))
	  (col (cdr pt)))
      (list (cons (+ 1 row) col)
	    (cons row (+ 1 col))
	    (cons (+ -1 row) col)
	    (cons row (+ -1 col)))))

  (let ((next '())
	(done (aoc-make-grid (aoc-grid-rows grid) (aoc-grid-cols grid) #f)))
    (for-each (lambda (p)
		(aoc-grid-set! grid (car p) (cdr p) score)
		(for-each (lambda (pp)
			    (if (visit? (car pp) (cdr pp) done)
				(set! next (cons pp next))))
			  (neighbors p)))
	      todo)
    (if (not (null? next))
	(mark grid (+ 1 score) next))))

(define (day18-simulate grid input seconds)
  (let loop ((input input) (seconds seconds) (prev '()))
    (if (zero? seconds)
	prev
	(let ((x (caar input))
	      (y (cadar input)))
	  (aoc-grid-set! grid y x #\#)
	  (loop (cdr input) (+ -1 seconds) (car input))))))
  
(define (day18-read input)
  (with-input-from-file input
    (lambda ()
      (let loop ((coords (read)) (acc '()))
	(if (eof-object? coords)
	    (reverse acc)
 	    (loop (read) (cons coords acc)))))))

(define (day18-test)
  (let ((grid (aoc-make-grid 7 7 #\.))
	(input (day18-read "test.18")))
    (day18-simulate grid input 12)
    (mark grid 0 (cons (cons 0 0) '()))
    (aoc-print-grid grid " ")
    ))

(define (day18-part1)
  (let ((grid (aoc-make-grid 71 71 #\.))
	(input (day18-read "input.18")))
    (day18-simulate grid input 1024)
    (mark grid 0 (cons (cons 0 0) '()))
    (aoc-print-grid grid " ")))

(define (day18-part2)
  (let ((input (day18-read "input.18")))
    (let loop ((n 1025))
      (let* ((grid (aoc-make-grid 71 71 #\.))
	     (last-point (day18-simulate grid input n)))
	(mark grid 0 (cons (cons 0 0) '()))
	(if (equal? #\. (aoc-grid-ref grid 70 70))
	    (begin (display last-point) (newline))
	    (loop (+ 1 n)))))))

(day18-part2)
;;(day18-test)

