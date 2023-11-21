;;; day3.el - advent of code 2015
;;; Friday, November 17, 2023
;;; (practicing for this year)

;; part 1
(aoc-copy-output
 (let ((chars (aoc-buffer-chars "day3.2015.txt"))
       (map (make-hash-table :test 'equal)))
   (let ((lat 0)
	 (long 0))
     (setf (gethash (cons lat long) map) 1)
     (dolist (dir chars)
       (cond ((eql ?^ dir) (cl-incf lat))
	     ((eql ?v dir) (cl-decf lat))
	     ((eql ?> dir) (cl-decf long))
	     ((eql ?< dir) (cl-incf long)))
       (cl-incf (gethash (cons lat long) map 0)))
     (let ((count 0))
       (maphash (lambda (k v) (cl-incf count)) map)
       count))))

;; part 2
(defclass house-map ()
  ((lat :initform 0)
   (long :initform 0)
   (map :initform (make-hash-table :test 'equal))))

(cl-defmethod map-store-pos ((map house-map))
  (let ((lat (slot-value map 'lat))
	(long (slot-value map 'long)))
    (cl-incf (gethash (cons lat long) (slot-value map 'map) 0))))

(cl-defmethod map-move ((map house-map) dir)
  (map-store-pos map)
  (cond ((eql ?^ dir) (cl-incf (slot-value map 'lat)))
	((eql ?v dir) (cl-decf (slot-value map 'lat)))
	((eql ?> dir) (cl-decf (slot-value map 'long)))
	((eql ?< dir) (cl-incf (slot-value map 'long))))
  (map-store-pos map))

(aoc-copy-output ()
 (let ((chars (aoc-buffer-chars "day3.2015.txt"))
       (santa (make-instance 'house-map))
       (robot (make-instance 'house-map)))
   (while chars
     (map-move santa (pop chars))
     (map-move robot (pop chars)))
   (let ((combined (make-hash-table :test 'equal)))
     (maphash (lambda (k v) (setf (gethash k combined) 1))
	      (slot-value robot 'map))
     (maphash (lambda (k v) (setf (gethash k combined) 1))
	      (slot-value santa 'map))
     (let ((count 0))
       (maphash (lambda (k v) (cl-incf count)) combined)
       count))))
   
