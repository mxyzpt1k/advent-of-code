;;; Advent of Code
;;; Friday, December 20, 2024

;;       --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;;  20   00:45:41   2408      0          -      -      -

(load "aoc.scm")

(define (mark grid score todo)
  
  (define (visit? row col done)
    (cond ((or (> 0 row) (> 0 col)) #f)
	  ((or (= (aoc-grid-rows grid) row) (= (aoc-grid-cols grid) col)) #f)
	  ((number? (aoc-grid-ref grid row col)) #f)
	  ((aoc-grid-ref done row col) #f)
	  (else (aoc-grid-set! done row col #t)
		(let ((ch (aoc-grid-ref grid row col)))
		  (or (char=? #\. ch) (char=? #\E ch))))))
  
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
	(mark grid (+ 1 score) next)
	score)))

(define (find-cheats g r c acc)
  (let* ((gref aoc-grid-ref)
	 (left (lambda () (gref g r (+ -1 c))))
	 (right (lambda () (gref g r (+ 1 c))))
	 (up (lambda () (gref g (+ -1 r) c)))
	 (down (lambda () (gref g (+ 1 r) c))))
    (cond ((= (+ 1 r) (aoc-grid-rows g)) #f)
	  ((= (+ 1 c) (aoc-grid-cols g)) (find-cheats g (+ 1 r) 1 acc))
	  ((number? (gref g r c)) (find-cheats g r (+ 1 c) acc))
	  (else (if (and (number? (left)) (number? (right)))
		    (acc (abs (- (left) (right)))))
		(if (and (number? (up)) (number? (down)))
		    (acc (abs (- (up) (down)))))
		(find-cheats g r (+ 1 c) acc)))))

(let* ((grid (aoc-read-grid "input.20"))
       (start (aoc-grid-find grid (lambda (c) (char=? c #\S))))
       (count 0)
       (counter (lambda (n) (if (< 99 (- n 2)) (set! count (+ 1 count))))))
  (mark grid 0 start)
  (find-cheats grid 1 1 counter)
  count)
