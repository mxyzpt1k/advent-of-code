;;; Day 15: Lens Library
;;; Advent of Code 2023
;;; Friday, December 15, 2023

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  15   00:28:37   6451      0   01:10:35   5676      0

;;; part 1

(defun day15-hash (string)
  (seq-reduce (lambda (hash c)
		(if (= c ?\n)
		    hash
		  (mod (* 17 (+ hash c)) 256)))
	      string 0))

(defun day15-part-1 (buffer-name)
  (with-current-buffer buffer-name
    (seq-reduce '+ (mapcar 'day15-hash (string-split (buffer-string) ",")) 0)))

;; (day15-part-1 "day15.2023.input.txt")


;;; part 2

(defun day15-part-2 (buffer-name)
  (with-current-buffer buffer-name
    (let ((*hashmap* (make-vector 256 ())))
      (dolist (lens (string-split (buffer-string) ","))
	(if (seq-contains-p lens ?=)
	    (day15-add lens)
	  (day15-remove lens)))
	(day15-score))))

;; (day15-part-2 "test.buff")
;; (day15-part-2 "day15.2023.input.txt")

(defun day15-score ()
  (let ((total 0))
    (dotimes (box (length *hashmap*) total)
      (let ((slot-num 0))
	(dolist (slot (reverse (aref *hashmap* box)))
	  (cl-incf total (* (1+ box) (cl-incf slot-num) (cdr slot))))))))

(defun day15-remove (lens)
  (seq-let (tag empty) (string-split lens "-")
    (let ((hash (day15-hash tag)))
      (let ((alist (aref *hashmap* hash)))
	(unless (null alist)
	  (let ((pair (assoc tag alist)))
	    (unless (null pair)
	      (setf (aref *hashmap* hash)
		    (seq-remove (lambda (p) (equal (car p) tag)) alist)))))))))
	    
(defun day15-add (lens)
  (seq-let (tag len) (string-split lens "=")
    (setq len (string-to-number len))
    (let* ((hash (day15-hash tag))
	   (alist (aref *hashmap* hash)))
      (if (null alist)
	  (setf (aref *hashmap* hash) (list (cons tag len)))
	(let ((pair (assoc tag alist)))
	  (if (null pair)
	      (setf (aref *hashmap* hash) (cons (cons tag len) alist))
	    (setcdr pair len)))))))
