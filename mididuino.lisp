(in-package :gcode)

(defun maennchen-trace-files (images &key (scale 0.4))
  (with-program ("potrace")
    (with-tool (*plywood-board-tool*)
      (spindle-on)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
      
      (let* ((panels (mapcar #'(lambda (x) (calculate-panel-code
					    `((maennchen-trace-panel ,x :scale ,scale))))
			     images))
	     (orders (order-panels panels '((1 2 3)
					    (4 5 6)) 10)))
	(loop for order in orders
	   for x = (second order)
	   for y = (third order)
	   for panel = (first order)
	   do (schedule-panel panel x y))))))

(defun hubble-circle (&key (x 5) (y 5))
  (let ((width 7))
    (with-named-pass ("mill")
      (goto-abs :x x :y (- y (/ width 2)))
      (repeat-for-depth (4)
			(circle (/ width 2))))
    (with-named-pass ("drill")
      (drill :x x :y y :diameter 3 :depth 4))))

(defun make-panel-array (num x y)
  (make-list y :initial-element (make-list x :initial-element num)))

(defun hubble-program ()
  (let ((panels (list (calculate-panel 'hubble-circle))))
    
  (with-program ("hubble")
    (with-tool (*plywood-board-tool*)
      (spindle-on)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
      (let ((orders (order-panels panels (make-panel-array 1 10 6) 3)))
	(loop for order in orders
	   for x = (second order)
	   for y = (third order)
	   for panel = (first order)
	     do (schedule-panel panel x y)))))))


(defun maennchen-trace-panel (num &key (scale 0.4) cache)
  (let* ((image (format nil "/Users/manuel/siff-svn/ruinwesen/mididuino-boards/boards/board-eyes-~A.png" num))
	 (outline (format nil "/Users/manuel/siff-svn/ruinwesen/mididuino-boards/boards/board-outline-~A.png" num))
	 (image2 (format nil "/Users/manuel/siff-svn/ruinwesen/mididuino-boards/boards/board-logo-~A.png" num))
	 (curves-outline (or (and cache *outline*)
			     (mapcar #'curve-to-arcs
				     (setf *outline-bezier*
					   (trace-image outline :not-colors (list #xFFFFFF
										  #xFFFEFE))))))
	 (curves-eyes (or (and cache *eyes*)
			  (mapcar #'curve-to-arcs
				  (setf *eyes-bezier*
					(trace-image image :not-colors (list #xFFFFFF))))))
	 
	 (curves-name (or (and cache *name*)
			  (mapcar #'curve-to-arcs
				  (setf *name-bezier*
					(trace-image image2 :not-colors (list #xFFFFFF)))))))
    (format t "image~%")
    (image-colors image)
    (format t "outline~%")
    (image-colors outline)
    (format t "image2~%")
    (image-colors image2)
    (setf curves-outline (remove-if #'(lambda (x)
					(bbox-below-p x 2)) curves-outline))
    (setf curves-eyes (remove-if #'(lambda (x)
					(bbox-below-p x 2)) curves-eyes))
    (setf curves-name (remove-if #'(lambda (x)
					(bbox-below-p x 2)) curves-name))

    (setf *outline* curves-outline)
    (setf *eyes* curves-eyes)
    (setf *name* curves-name)
    (format t "cmplete bbox: ~A~%" (bounding-box (append curves-outline curves-eyes)))
;;    (setf *eye* (first curves-eyes))

    (let ((bbox (bounding-box (append curves-outline curves-eyes))))
      (with-tool (*plywood-board-tool*)

	(with-named-pass ("drills")
	  (with-tool (*plywood-board-tool*)
	    (with-transform ((translation-matrix 7 60))
;;	      (p5-rect :x 10 :y 10 :width 153 :height 84.6)
	      
	      (drill :x 10 :y 10 :diameter 3 :depth 4)
	      (drill :x (+ 10 153) :y 10
		     :diameter 3 :depth 4)
	      (drill :x (+ 10 153) :y (+ 10 84.6) :diameter 3 :depth 4)
	      (drill :x 10 :y (+ 10 84.6) :diameter 3 :depth 4)
	      
	      (drill :x 8 :y 0 :diameter 3 :depth 4)
	      (drill :x 10 :y -36 :diameter 3 :depth 4)
	      (drill :x 87 :y -36 :diameter 3 :depth 4)
	      (drill :x 89 :y 0 :diameter 3 :depth 4)
	      )))
	  
	
	(with-transform ((translation-matrix (* scale (- (2d-point-x (line-a bbox))))
					     (* scale (- (2d-point-y (line-a bbox))))))

	  #-nil
	  (with-named-pass ("eyes")
	    (with-tool (*plywood-board-tool*)
	      (dolist (curve curves-eyes)
		(mill-curve (offset-curve curve 3) :depth 0.7 :scale scale)

	      #+nil(with-named-pass ("raster")
		(let ((box (bounding-box curve)))
		  (with-transform ((scaling-matrix scale))
		    (with-tool-down (2)
		      (dolist (seg (raster (offset-curve curve 3)
					   :start-y (floor (bbox-bottom box))
					   :end-y (ceiling (bbox-top box))))
			(fly-to :x (2d-point-x (object-start-point seg))
				:y (2d-point-y (object-start-point seg)))
			(mill-segment seg))))))

	      )))

	  (with-named-pass ("font")
	    (with-tool (*plywood-board-tool*)
	      (dolist (curve curves-name)
		(mill-curve curve :scale scale :depth 0.7))))

	  #-nil
	  (with-named-pass ("inner")
	    (with-tool (*plywood-board-tool*)
	      (mill-curve (offset-curve (first curves-outline) 3) :depth 0.7 :scale scale)))

	  #-nil
	  (with-named-pass ("outline")
	    (with-tool (*plywood-board-tool*)
	      (mill-curve (offset-curve (first curves-outline) -6) :scale scale :depth 4))))))))

;; board = 1035 pixel width
(defun test-eyes-offset (&key (scale 0.4))
  (with-program ("maennchen")
    (with-transform ((translation-matrix 0 4))
      (with-tool (*plywood-board-tool*)
	(let ((bbox (bounding-box (append *outline* *eyes*))))
	  (spindle-on)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)
	  (with-transform ((translation-matrix (* scale (- (2d-point-x (line-a bbox))))
					       (* scale (- (2d-point-y (line-a bbox))))))
	    
	    (with-named-pass ("eyes")
	      (let ((logo (first (last *eyes*))))
		(mill-curve *logo* :depth 2 :scale scale)))))))))


  

(defun maennchen-trace (num &key (scale 0.337) cache)
  (with-program ("maennchen")
    (with-transform ((translation-matrix 0 4))
      (with-tool (*plywood-board-tool*)
	(spindle-on)
	(goto-abs :x 0 :y 0)
	(goto-abs :z *fly-height*)
	(maennchen-trace-panel num :scale scale :cache cache)
	))))

