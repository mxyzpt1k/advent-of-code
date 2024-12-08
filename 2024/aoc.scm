;;; aoc.scm - utilities to help with advent of code
;;; started on Sunday, December 08, 2024

(define (aoc-read-line-vector)
  (let loop ((acc '()))
    (let ((c (read-char)))
      (cond ((eof-object? c) c)
	    ((char=? #\newline c) (list->vector (reverse acc)))
	    (else (loop (cons c acc)))))))

(define (aoc-read-grid file)
  (with-input-from-file file
    (lambda ()
      (let loop ((grid '()))
	(let ((row (aoc-read-line-vector)))
	  (if (eof-object? row)
	      (list->vector (reverse grid))
	      (loop (cons row grid))))))))

(define (aoc-grid-size grid)
  (list (vector-length grid) (vector-length (vector-ref grid 0))))

(define (aoc-make-grid rows cols char)
  (let ((grid (make-vector rows)))
    (aoc-do-times rows (lambda (r) (vector-set! grid r (make-vector cols char))))
    grid))

(define (aoc-copy-grid grid)
  (let* ((size (aoc-grid-size grid))
	 (new (aoc-make-grid (car size) (cadr size) #\.)))
    (aoc-walk-grid grid
      (lambda (g r c) (aoc-grid-set! new r c (aoc-grid-ref g r c))))
    new))

(define (aoc-expand-grid grid char)
  (let ((size (aoc-grid-size grid)))
    (let ((new (aoc-make-grid (+ 2 (car size)) (+ 2 (cadr size)) char)))
      (aoc-walk-grid grid
	(lambda (g r c)
	  (aoc-grid-set! new (+ 1 r) (+ 1 c) (aoc-grid-ref g r c))))
      new)))

(define (aoc-grid-ref grid row col)
  (let ((v (vector-ref grid row)))
    (vector-ref v col)))

(define (aoc-grid-set! grid row col val)
  (let ((v (vector-ref grid row)))
    (vector-set! v col val)))

(define (aoc-walk-grid grid fun)
  (let ((size (aoc-grid-size grid)))
    (aoc-do-times (car size)
      (lambda (row)
	(aoc-do-times (cadr size)
	  (lambda (col)
	    (fun grid row col)))))))

(define (aoc-do-times n fun)
  (let loop ((k 0))
    (cond ((= k n) #t)
	  (else (fun k)
		(loop (+ 1 k))))))

(define (aoc-tests)
  
  (let ((g (aoc-read-grid "test.8.2")))
    (let* ((count 0)
	   (counter (lambda (c)
		      (lambda (grid row col)
			(if (char=? c (aoc-grid-ref grid row col))
			    (set! count (+ 1 count)))))))
      (aoc-walk-grid g (counter #\T))
      count))

  (aoc-copy-grid (aoc-read-grid "test.8.2"))
  
  (aoc-expand-grid (aoc-read-grid "test.8.2") #\%)
  )
  


