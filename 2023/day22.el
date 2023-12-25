;;; Day 22: Sand Slabs 
;;; Advent of Code 2023
;;; Friday, December 22, 2023

(require 'peg)

(cl-defstruct cube id aside zside point$ above below)

(defvar cube-space (make-hash-table :test 'equal))

(defun day22-part1 (buffer-name)
  (let ((cube-space (make-hash-table :test #'equal))
	(cubes (cube-sort (day22-read-input buffer-name))))
    (seq-do #'day22-drop cubes)
    (seq-count #'cube-zappable? cubes)))

;; (day22-part1 "day22.2023.input.txt")
;; (day22-part1 "test.buff")

(defun day22-parse-cube ()
  (with-peg-rules
      ((cube    (bol) (list triple) "~" (list triple) (eol)
		`(a b -- (vconcat a) (vconcat b)))
       (triple  number "," number "," number)
       (number  (substring (+ (range ?0 ?9)))
		`(a -- (string-to-number a))))
    (peg-run (peg cube))))

(defun day22-read-input (buffer-name)
  (with-current-buffer buffer-name
    (goto-char (point-min))
    (let ((acc ())
	  (id 0)
	  (rec (day22-parse-cube)))
      (while rec
	(seq-let (aside zside) rec
	  (let ((cube (make-cube :id (cl-incf id) :aside aside :zside zside
				 :below () :above () :point$ ())))
	    (push cube acc)))
	(forward-line)
	(setq rec (day22-parse-cube)))
      (cube-space-update acc)
      acc)))

(defun cube-space-update (cubes)
  (seq-do (lambda (c)
	    (seq-do (lambda (xy)
		      (let ((list (gethash xy cube-space)))
			(setf (gethash xy cube-space) (cons c list))))
		    (cube-xy-list c)))
	  cubes))

(defun cube-x1 (c) (aref (cube-aside c) 0))
(defun cube-y1 (c) (aref (cube-aside c) 1))
(defun cube-z1 (c) (aref (cube-aside c) 2))
(defun cube-z1-set (c val)
  (let ((triple (cube-aside c)))
    (setf (aref triple 2) val)))

(defun cube-x2 (c) (aref (cube-zside c) 0))
(defun cube-y2 (c) (aref (cube-zside c) 1))
(defun cube-z2 (c) (aref (cube-zside c) 2))
(defun cube-z2-set (c val)
  (let ((triple (cube-zside c)))
    (setf (aref triple 2) val)))

(defun cube-top (cube) (max (cube-z1 cube) (cube-z2 cube)))
(defun cube-bottom (cube) (min (cube-z1 cube) (cube-z2 cube)))
(defun cube-bottom-set (c z)
  (let ((b (cube-bottom c)))
    (if (= b z)
	nil
      (cube-z1-set c (+ (cube-z1 c) (- z b)))
      (cube-z2-set c (+ (cube-z2 c) (- z b)))
      (setf (cube-point$ c) nil)
      t)))

(defun cube-points (c)
  (if (cube-point$ c)
      (cube-point$ c)
    (let ((points ()))
      (cond ((/= (cube-x1 c) (cube-x2 c))
	     (let ((start (min (cube-x1 c) (cube-x2 c)))
		   (end (max (cube-x1 c) (cube-x2 c))))
	       (push (vector start (cube-y1 c) (cube-z1 c)) points)
	       (dotimes (x (- end start))
		 (push (vector (+ x start 1) (cube-y1 c) (cube-z1 c)) points))))
	    ((/= (cube-y1 c) (cube-y2 c))
	     (let ((start (min (cube-y1 c) (cube-y2 c)))
		   (end (max (cube-y1 c) (cube-y2 c))))
	       (push (vector (cube-x1 c) start (cube-z1 c)) points)
	       (dotimes (y (- end start))
		 (push (vector (cube-x1 c) (+ y start 1) (cube-z1 c)) points))))
	    (t (let ((start (min (cube-z1 c) (cube-z2 c)))
		     (end (max (cube-z1 c) (cube-z2 c))))
		 (push (vector (cube-x1 c) (cube-y1 c) start) points)
		 (dotimes (z (- end start))
		   (push (vector (cube-x1 c) (cube-y1 c) (+ z start 1)) points)))))
      (setf (cube-point$ c) (reverse points)))))

(defun cube-push-above (above below)
  (unless (= (cube-id below) (cube-id above))
    (unless (member above (cube-above below))
      (setf (cube-above below) (cons above (cube-above below))))))

(defun cube-push-below (below above)
  (unless (= (cube-id above) (cube-id below))
    (unless (member below (cube-below above))
      (setf (cube-below above) (cons below (cube-below above))))))

(defun cube-xy-list (c)
  (seq-uniq (mapcar (lambda (p) (list (aref p 0) (aref p 1))) (cube-points c)) #'equal))
;; (cube-xy-list [[4 9 111] [7 9 111] 'id ()])
;; (cube-xy-list [[4 9 111] [4 9 118] 'id ()])

(defun cube-sort (cubes)
  (sort cubes (lambda (a b) (< (cube-bottom a) (cube-bottom b)))))
  
(defun all-cubes-below (c)
  (seq-filter (lambda (k) (< (cube-top k) (cube-bottom c))) ;k overlaps in xy with c
	      (seq-uniq (flatten-tree (mapcar (lambda (xy) (gethash xy cube-space)) (cube-xy-list c)))
			(lambda (a b) (= (cube-id a) (cube-id b))))))

(defun all-cubes-above (c)
  (seq-filter (lambda (k) (> (cube-bottom k) (cube-top c))) ;k overlaps in xy with c
	      (seq-uniq (flatten-tree
			 (mapcar (lambda (xy) (gethash xy cube-space)) (cube-xy-list c)))
			(lambda (a b) (= (cube-id a) (cube-id b))))))

(defun day22-drop (cube)
  (when (< 1 (cube-bottom cube))
    (let ((below (all-cubes-below cube)))
      (if (null below)
	  (cube-bottom-set cube 1)
	(let ((top (seq-max (mapcar #'cube-top below))))
	  (dolist (b below)
	    (when (= top (cube-top b))
	      (cube-push-above cube b)     ; tell b this cube is above b
	      (cube-push-below b cube)))   ; remember b is below (supports) this cube
	  (cube-bottom-set cube (1+ top)))))))

(defun cube-zappable? (c)
  ;; a cube is zappable if there is nothing resting on it
  ;; of if something else supports any cube resting on it
  (if (null (cube-above c))
      t
    (let ((zappable t))
      (dolist (above (cube-above c) zappable)
	(let ((below (cube-below above)))
	  (when (null (cdr below))  ; (= 1 (length below))
	    (setq zappable nil)))))))

;; part 2

(defun cube-sole-support-p (below above)
  (let ((supports (cube-below above)))
    (and (member below supports)
	 (null (cdr supports)))))

(defun cube-has-support? (cube missing)
  (if (= 1 (cube-bottom cube))
      (cube-id cube)
    (and (seq-difference (cube-below cube) missing) (cube-id cube))))

(defun day22-collect-above (c)
  (let ((above (cube-above c)))
    (if (null above)
	above
      (append above (seq-uniq (flatten-tree (mapcar #'day22-collect-above above)))))))

(defun day22-chain-reaction (cube)
  ;; if this cube is zappable from part1, it's not counted
  ;; how many bricks above cube would fall if cube was missing?  
  (let ((chain (day22-collect-above cube))
	(remove ())
	(done nil))
    (while (and chain (not done))
      (dolist (k chain)
	(setq remove (append (seq-difference (cube-below k) chain) remove)))
      (if (null remove)
	  (setq done t)
	(setq chain (seq-difference chain remove))
	(setq remove nil)))
    chain))
       
(defun day22-part2 (buffer-name)
  (let ((cubes (cube-sort (day22-read-input buffer-name)))
	(score 0))
    (seq-do #'day22-drop cubes)
    (dolist (c cubes score)
      (when (= 1 (cube-id c))
	(cl-incf score (cube-chain-reaction c ()))))))

(defun day22-test (buffer-name)
  (let ((cubes (cube-sort (day22-read-input buffer-name)))
	(score 0))
    (seq-do #'day22-drop cubes)
    (let ((one (seq-find (lambda (k) (= 1 (cube-id k))) cubes)))
      (day22-chain-reaction one))))

;;(day22-test "test.buff")

;; (message "percent complete: %d, score: %d, eta: %d minutes"
;; 	 (* 100 (/ (+ 1.0 k) n-cubes))
;; 	 total
;; 	 (day22-eta start (1+ k) n-cubes)))))
    
;; (day22-part2 "test.buff")
;; (benchmark-run (aoc-copy-output () (day22-part2 "day22.2023.input"

(defun day22-eta (start done n-cubes)
  (let ((complete (/ (float done) n-cubes))
	(elapsed (- (float-time) start)))
    (let ((expected-time (* elapsed (/ n-cubes done))))
      (/ (- expected-time elapsed) 60))))
