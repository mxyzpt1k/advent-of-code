;;; Advent of code
;;; Tuesday, December 17, 2024

;; for scheme48
;; > ,open bitwise
;; > (define logxor bitwise-xor)

(define (octal-digits n)
  (let loop ((n n) (acc '()))
    (cond ((< n 8) (cons n acc))
	  (else (loop (quotient n 8) (cons (remainder n 8) acc))))))

(define (octal-digits->num digits)
  (let loop ((digits digits) (num 0))
    (cond ((null? digits) num)
	  (else (loop (cdr digits) (+ (car digits) (* 8 num)))))))

(define (print x)
  (display x)
  (newline)
  x)

(define (chronospatial-computer regs prog)
  
  (define (get-A) (vector-ref regs 0))
  (define (set-A v) (vector-set! regs 0 v))
  (define (get-B) (vector-ref regs 1))
  (define (set-B v) (vector-set! regs 1 v))
  (define (get-C) (vector-ref regs 2))
  (define (set-C v) (vector-set! regs 2 v))
  
  (define (combo op)
    (case op
      ((0 1 2 3) op)
      ((4) (get-A))
      ((5) (get-B))
      ((6) (get-C))
      ((7) (print "invalid operator (7)"))))
  
  (define (correct? output expected)
    (cond ((null? output) #t)
	  ((null? expected) #f)
	  ((not (= (car output) (car expected))) #f)
	  (else (correct? (cdr output) (cdr expected)))))
  ;;(define (correct? a b) #t)

  (let ((pc 0)
	(output '())
	(expected (vector->list prog)))

    (define (adv op) (set-A (quotient (get-A) (expt 2 (combo op)))))
    (define (bxl op) (set-B (logxor (get-B) op)))
    (define (bst op) (set-B (modulo (combo op) 8)))
    (define (jnz op) (if (not (zero? (get-A))) (set! pc (+ -2 op))))
    (define (bxc op) (set-B (logxor (get-B) (get-C))))
    (define (out op) (set! output (cons (modulo (combo op) 8) output)))
    (define (out-p2 op)
      (out op)
      (if (not (correct? (reverse output) expected))
	  (set! pc (vector-length prog))))
    (define (bdv op) (set-B (quotient (get-A) (expt 2 (combo op)))))
    (define (cdv op) (set-C (quotient (get-A) (expt 2 (combo op)))))
    
    (define (inst pc)
      (case (vector-ref prog pc)
	((0) adv)
	((1) bxl)
	((2) bst)
	((3) jnz)
	((4) bxc)
	((5) out)
	((6) bdv)
	((7) cdv)))

    (let run ()
      (if (< pc (vector-length prog))
	  (begin
	    ((inst pc) (vector-ref prog (+ 1 pc)))
	    (set! pc (+ 2 pc))
	    (run))))

    ;; part 1
    ;; (let print ((data (reverse output)))
    ;;   (cond ((null? data) (newline))
    ;; 	    (else (display (car data))
    ;; 		  (display #\,)
    ;; 		  (print (cdr data)))))

    ;; part 2
    ;;(and (= (length output) (length expected)) (correct? (reverse output) expected))
     output
    ))

(define (part2 regs prog)
  (let loop ((n 0))
    (vector-set! regs 0 n)
    (vector-set! regs 1 0)
    (vector-set! regs 2 0)
    (cond ;;((< 200000 n) #f)
	  ((not (chronospatial-computer regs prog)) (loop (+ 1 n)))
	  (else (display n)
		(newline)))))

(define (octal-bits n)
  (case o
    ((0) '(0 0 0))
    ((1) '(0 0 1))
    ((2) '(0 1 0))
    ((3) '(0 1 1))
    ((4) '(1 0 0))
    ((5) '(1 0 1))
    ((6) '(1 1 0))
    ((7) '(1 1 1))
    (else 'not-octal-digit)))

(define (mappend fun lst)
  (cond ((null? lst) '())
	(else (append (fun (car lst)) (mappend fun (cdr lst))))))
	  
(define (solve x)
  (let ((r (vector 21539243 0 0))
	(p (vector 2 4 1 3 7 5 1 5 0 3 4 1 5 5 3 0)))
    (vector-set! r 0 x)
    (let ((y (chronospatial-computer r p)))
      ;;(print y)
      ;;(print (list 'input x 'output (mappend octal-bits y)))
      ;; (print (list p (octal (vector->list p))))
      ;; (print (list p (octal (reverse (vector->list p)))))
      ;; (print (list x '> y (octal y)))
      ;; (print (list x '< y (octal (reverse y))))
      ;; (print (list (vector-length p) (length y)))
      ;; (print (list 'target (vector->list p)))
      (octal-digits->num y))))

(define (repeat d times)
  (let loop ((n times) (acc '()))
    (if (zero? n)
	acc
	(loop (+ -1 n) (cons d acc)))))

(define (feeble)
  (let ((target (octal-digits->num `(2 4 1 3 7 5 1 5 0 3 4 1 5 5 3 0))))
    (let loop ((n (octal-digits->num (repeat 1 14))) (p 0))
      (let ((s (solve n)))
	(if (< s (- p 100))
	    (print (list (+ -1 n) p n s (- s p))))
	(loop (+ 45940736 n) s)))))

(feeble)

;;(- 628294258687 628293996543)
;;(- 628293996543 628293734399)
;;(- 628293734399 628293472255)
