;;; aoc.scm - utilities to help with advent of code
;;; started on Sunday, December 08, 2024

(define (aoc-read-line-vector)
  (let loop ((acc '()))
    (let ((c (peek-char)))
      (cond ((eof-object? c) (if (null? acc)
				 c
				 (list->vector (reverse acc))))
	    ((char=? #\newline c) (read-char) (list->vector (reverse acc)))
	    (else (loop (cons (read-char) acc)))))))

(define (aoc-read-grid file)
  (with-input-from-file file
    (lambda ()
      (let loop ((grid '()))
	(let ((row (aoc-read-line-vector)))
	  (if (eof-object? row)
	      (list->vector (reverse grid))
	      (loop (cons row grid))))))))

(define (aoc-grid-size grid)
  (cons (vector-length grid) (vector-length (vector-ref grid 0))))

(define (aoc-make-grid rows cols char)
  (let ((grid (make-vector rows)))
    (aoc-do-times rows
      (lambda (r) (vector-set! grid r (make-vector cols char))))
    grid))

(define (aoc-copy-grid grid)
  (let* ((size (aoc-grid-size grid))
	 (new (aoc-make-grid (car size) (cdr size) #\.)))
    (aoc-walk-grid grid
      (lambda (g r c) (aoc-grid-set! new r c (aoc-grid-ref g r c))))
    new))

(define (aoc-expand-grid grid char)
  (let ((size (aoc-grid-size grid)))
    (let ((new (aoc-make-grid (+ 2 (car size)) (+ 2 (cdr size)) char)))
      (aoc-walk-grid grid
	(lambda (g r c)
	  (aoc-grid-set! new (+ 1 r) (+ 1 c) (aoc-grid-ref g r c))))
      new)))

(define (aoc-reverse-vector vec)
  (list->vector (reverse (vector->list vec))))

;; a flip is north/south mirror
(define aoc-flip-grid aoc-reverse-vector)

;; a mirrot is an east/west flip
(define (aoc-mirror-grid grid)
  (let ((new (aoc-copy-grid grid)))
    (aoc-do-times (car (aoc-grid-size grid))
      (lambda (n)
	(vector-set! new n (aoc-reverse-vector (vector-ref grid n)))))
    new))

(define (aoc-tranpose-grid grid)
  (let ((size (aoc-grid-size grid)))
    (let ((new (aoc-make-grid (cdr size) (car size) #\.)))
      (aoc-walk-grid grid
	(lambda (g r c)
	  (aoc-grid-set! new c r (aoc-grid-ref grid r c))))
      new)))

(define (aoc-rotate-grid-left grid)
  (aoc-flip-grid (aoc-tranpose-grid grid)))

(define (aoc-rotate-grid-right grid)
  (aoc-mirror-grid (aoc-tranpose-grid grid)))

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
	(aoc-do-times (cdr size)
	  (lambda (col)
	    (fun grid row col)))))))

(define (aoc-grid-find grid pred?)
  (let ((acc '()))
    (aoc-walk-grid grid
      (lambda (g r c)
	(if (pred? (aoc-grid-ref g r c))
	    (set! acc (cons (cons r c) acc)))))
    acc))
 
(define (aoc-do-times n fun)
  (let loop ((k 0))
    (cond ((= k n) #t)
	  (else (fun k)
		(loop (+ 1 k))))))

(define (aoc-print-grid grid)
  (aoc-walk-grid grid
    (lambda (g r c)
      (if (zero? c) (newline))
      (display (aoc-grid-ref g r c))))
  (newline))

(define (aoc-tests)
  
  (let ((g (aoc-read-grid "test.8.2")))
    (let* ((count 0)
	   (counter (lambda (c)
		      (lambda (grid row col)
			(if (char=? c (aoc-grid-ref grid row col))
			    (set! count (+ 1 count)))))))
      (aoc-walk-grid g (counter #\T))
      count))

  (aoc-grid-find (aoc-read-grid "test.8.2") (lambda (c) (char=? c #\T)))

  (aoc-copy-grid (aoc-read-grid "test.8.2"))
  
  (aoc-print-grid (aoc-expand-grid (aoc-read-grid "test.8.2") #\%))
  )

;; Local Variables:
;; eval: (put 'aoc-do-times 'scheme-indent-function 'defun)
;; eval: (put 'aoc-walk-grid 'scheme-indent-function 'defun)
;; End: