;;; Day 25: Snowverload
;;; Advent of Code 2023
;;; Monday, December 25, 2023

;;; used graphviz/neato to find the edges to remove and got my best
;;; ranking of the month for part 1

;;;       --------Part 1--------   --------Part 2--------
;;; Day       Time   Rank  Score       Time   Rank  Score
;;;  25   01:28:13   1967      0          -      -      -

(defun day25-read-input (buffer-name)
  (let ((graph ()))
    (dolist (line (aoc-buffer-lines buffer-name) (seq-uniq graph))
      (let ((tokens (string-split line)))
	(let ((key (string-replace ":" "" (car tokens))))
	  (dolist (val (cdr tokens))
	    (push (cons key val) graph)))))))

(defun day25-part1 (buffer-name)
  (let ((edges (seq-remove (lambda (edge)
			     (or (equal edge `("kdk" . "nct"))
				 (equal edge `("nct" . "kdk"))
				 (equal edge `("cvx" . "tvj"))
				 (equal edge `("tvj" . "cvx"))
				 (equal edge `("fsv" . "spx"))
				 (equal edge `("spx" . "fsv"))))
			   (day25-read-input buffer-name))))
    (let ((graph (make-hash-table :test 'equal)))
      (dolist (edge edges)
	(setf (gethash (car edge) graph) (cons (cdr edge) (gethash (car edge) graph nil)))
	(setf (gethash (cdr edge) graph) (cons (car edge) (gethash (cdr edge) graph nil))))
      (let ((a (count-connections "nct" (make-hash-table :test 'equal)))
	    (b (count-connections "kdk" (make-hash-table :test 'equal))))
	(* a b)))))

;; (aoc-copy-output () (day25-part1 "day25.2023.input.txt"))

(defun count-connections (node seen)
  ;; starting at node A, count how many nodes A is connected to
  (let ((zs (gethash node graph)))
    (while zs
      (let ((z (pop zs)))
	(unless (gethash z seen)
	  (setf (gethash z seen) t)
	  (setq zs (append zs (gethash z graph))))))
    (hash-table-count seen)))

    

