(in-package :gcode)

(defparameter *voronoi-points*
  (loop for i from 0 to 10
       collect (2dp (p5-random -10 10)
		    (p5-random -10 10))))

(defun voronoi (points)
  (let ((points-sorted (sort (copy-tree points)
			     #'< :key #'2d-point-x)))
    
		