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
      (cond ((< id 0) "error")
	    ((= id (vector-length data)) (newline) csum)
	    ((even? id) (let file-loop ((size (vector-ref data id)) (p pos))
			  (cond ((zero? size) (loop (+ 1 id) p end))
				(else (set! csum (+ csum (* p (div id 2))))
				      (file-loop (+ -1 size) (+ 1 p))))))
	    (else (let compact ((space (vector-ref data id)) (p pos) (last end))
		    (cond ((< last id) (loop (+ 1 id) p last))
			  ((zero? space) (loop (+ 1 id) p last))
			  ((zero? (vector-ref data last)) (compact space p (+ -2 last)))
			  (else (set! csum (+ csum (* p (div last 2))))
				(vector-set! data last (+ -1 (vector-ref data last)))
				(compact (+ -1 space) (+ 1 p) last)))))))))

;; debugging values that were too high
;;(string-length "0099811188827773336446555566..............")
;;(string-length "0099811188827773336446555566")
;;                xx998xxx888x777xxx6xx6xxxx6x65
;;                009981118882777333644655556665

;;(day9-part2 (list->vector (map char-value (string->list "2333133121414131402"))))

(let ((data (with-input-from-file "day9.2024.input.txt"
	      aoc-read-line-vector)))
  (day9-part1 (vector-map char-value data)))

;; not 24943685000338
