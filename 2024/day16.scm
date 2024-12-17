;;; Advent of Code
;;; Monday, December 16, 2024

;;(load "aoc.scm")

(define best 0)

(define all-paths '())

(define (navigate grid row col dir score seen path)

  (define (seen?)
    (let* ((cell (aoc-grid-ref seen row col))
	   (cd (assq dir cell)))
      (and cd (< (cdr cd) score))))

  (define (update-seen)
    (let* ((cell (aoc-grid-ref seen row col))
	   (cd (assq dir cell)))
      (if cd
	  (set-cdr! cd score)
	  (aoc-grid-set! seen row col (cons (cons dir score) cell)))))
  
  (define (left dir)
    (case dir
      ('east 'north)
      ('north 'west)
      ('west 'south)
      ('south 'east)))

  (let ((path (cons (cons row col) path)))
    (cond ((char=? #\# (aoc-grid-ref grid row col)) #f)
	  ((char=? #\E (aoc-grid-ref grid row col))
	   (display score)
	   (newline)
	   (cond ((< score best) (set! all-paths path))
		 ((= score best) (set! all-paths (append path all-paths))))
	   (set! best (min score best)))
	  ((< best score) #f)
	  ((seen?) #f)
	  (else (update-seen)
		(if (eq? 'south dir)
		    (navigate grid (+ 1 row) col dir (+ 1 score) seen path))
		(if (eq? 'north dir)
		    (navigate grid (+ -1 row) col dir (+ 1 score) seen path))
		(if (eq? 'east dir)
		    (navigate grid row (+ 1 col) dir (+ 1 score) seen path))
		(if (eq? 'west dir)
		    (navigate grid row (+ -1 col) dir (+ 1 score) seen path))
		(navigate grid row col (left dir) (+ 1000 score) seen path)
		(navigate grid row col (left (left (left dir))) (+ 1000 score) seen path)
		(navigate grid row col (left (left dir)) (+ 2000 score) seen path)
		))))

(define (part1 grid)
  (let ((start-row 0)
	(start-col 0)
	(seen (aoc-copy-grid grid)))
    (aoc-walk-grid grid
      (lambda (g r c)
	(aoc-grid-set! seen r c '())
	(cond ((char=? #\S (aoc-grid-ref g r c))
	       (set! start-row r)
	       (set! start-col c)))))
    (navigate grid start-row start-col 'east 0 seen '())))

(define (part2 lst)
  (define (count-uniq lst)
    (cond ((null? lst) 0)
	  ((member (car lst) (cdr lst)) (count-uniq (cdr lst)))
	  (else (+ 1 (count-uniq (cdr lst))))))
  (count-uniq lst))  

(define (solve file)
  (let ((grid (aoc-read-grid file)))
    ;;(aoc-print-grid grid)
    (set! best (+ (* 1000 (vector-length grid)) (vector-length grid)))
    (set! all-paths '())
    (part1 grid)
    (part2 all-paths)))

;; (solve "test.16")
