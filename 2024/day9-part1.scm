;;; Advent of Code
;;; Monday, December 09, 2024

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;   9   02:09:52  11507      0   10:21:50  20775      0

(define (char-value c)
  (- (char->integer c) (char->integer #\0)))

(define (day9-part1 data)
  (let ((csum 0))
    (let loop ((id 0) (pos 0) (end (+ -1 (vector-length data))))
      
      (define (file-loop size p)
	(cond ((zero? size) (loop (+ 1 id) p end))
	      (else (set! csum (+ csum (* p (quotient id 2))))
		    (file-loop (+ -1 size) (+ 1 p)))))
      
      (define (compact space p last)
	(cond ((< last id) (loop (+ 1 id) p last))
	      ((zero? space) (loop (+ 1 id) p last))
	      ((zero? (vector-ref data last)) (compact space p (+ -2 last)))
	      (else (set! csum (+ csum (* p (quotient last 2))))
		    (vector-set! data last (+ -1 (vector-ref data last)))
		    (compact (+ -1 space) (+ 1 p) last))))
      
      (cond ((= id (vector-length data)) (newline) csum)
	    ((even? id) (file-loop (vector-ref data id) pos))
	    (else (compact (vector-ref data id) pos end))))))
    

;; debugging values that were too high
;;(string-length "0099811188827773336446555566..............")
;;(string-length "0099811188827773336446555566")
;;                xx998xxx888x777xxx6xx6xxxx6x65
;;                009981118882777333644655556665

;;(day9-part1 (list->vector (map char-value (string->list "2333133121414131402"))))

(let ((data (with-input-from-file "day9.2024.input.txt"
	      aoc-read-line-vector)))
  (day9-part1 (list->vector (map char-value (vector->list data)))))

;; not 24943685000338
