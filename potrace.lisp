(in-package :gcode)

(defun goto-point (p)
  (goto-abs :x (2d-point-x p)
	    :y (2d-point-y p)))

(defun mill-point (p)
  (mill-abs :x (2d-point-x p)
	    :y (2d-point-y p)))


(defun interpret-bezier (bezier)
  (with-slots (a b u v) bezier
    (mill-point a)
    (dotimes (i 100)
      (let ((p (eval-bezier bezier (/ i 100.0))))
	(mill-point p)))
    (mill-point b)))

(defun split-potrace-test (potrace)
  (let (res)
    (loop for i in potrace
       do (cond ((typep i 'bezier)
		 (let ((splits (split-bezier-times i '(0.3 0.6))))
		   (dolist (i splits)
		     (push i res))
		   ))
		(t (push i res))))
    (nreverse res)))

(defun interpret-bezier-arcs (bezier)
  (with-slots (a b u v) bezier
    (mill-point a)
    (let ((arcs (bezier-to-arc bezier)))
      (mill-curve arcs)
      (dolist (arc arcs)
	  (with-slots (a b centre direction) arc
	    (cond ((eq direction :cw)
		   (arc-cw  :x (2d-point-x b) :y (2d-point-y b)
			    :i (2d-point-x centre) :j (2d-point-y centre)))
		  ((eq direction :ccw)
		   (arc-ccw :x (2d-point-x b) :y (2d-point-y b)
			    :i (2d-point-x centre) :j (2d-point-y centre))))
	    )))
    
    (mill-point b)))

(defun mill-segment (seg)
  (cond ((typep seg 'line)
	 (mill-abs :x (2d-point-x (line-b seg))
		   :y (2d-point-y (line-b seg))))
	((typep seg 'arc)
	 (with-slots (a b centre direction) seg
	   (cond ((eq direction :cw)
		  (arc-cw  :x (2d-point-x b) :y (2d-point-y b)
			   :i (2d-point-x centre) :j (2d-point-y centre)))
		 ((eq direction :ccw)
		  (arc-ccw :x (2d-point-x b) :y (2d-point-y b)
			   :i (2d-point-x centre) :j (2d-point-y centre))))))))

(defun mill-curve (curve &key depth (scale 1))
  (let ((point (object-start-point (first curve))))
    (with-transform ((scaling-matrix scale))
      (fly-to :x (2d-point-x point) :y (2d-point-y point))))

  (repeat-for-depth ((or depth (tool-depth *current-tool*)))
    (with-transform ((scaling-matrix scale))
      (dolist (seg curve)
	(mill-segment seg)))))

(defun interpret-curve (CurvE)
  (with-program ("potrace")
    (with-tool (*pcb-tool*)
      (with-named-pass ("mill")
	(mill-curve curve)))))

(defun test-curve (curve)
  (with-program ("potrace")
    (with-tool (*pcb-tool*)
      (with-named-pass ("mill")
	(with-transform ((scaling-matrix 0.2))
;;	  (mill-curve curve)
	  (format t "length: ~A~%" (length curve))
	  (mill-curve (offset-curve curve 3))
	  (format t "length: ~A~%" (length curve))
	  (mill-curve (offset-curve curve -3))
	  (format t "length: ~A~%" (length curve))

	  )))))

(defgeneric bounding-box (object))

#+nil
(defmethod bounding-box ((p point))
  (make-line :a p :b p))

(defmethod bounding-box ((l line))
  (with-slots (a b) l
    (let ((min-x (min (2d-point-x a) (2d-point-x b)))
	  (min-y (min (2d-point-y a) (2d-point-y b)))
	  (max-x (max (2d-point-x a) (2d-point-x b)))
	  (max-y (max (2d-point-y a) (2d-point-y b))))
      (make-line :a (2dp min-x min-y) :b (2dp max-x max-y)))))

(defmethod bounding-box ((a arc))
  ;; XXX billig, stimmt nicht
  (with-slots (a b) a
    (let ((min-x (min (2d-point-x a) (2d-point-x b)))
	  (min-y (min (2d-point-y a) (2d-point-y b)))
	  (max-x (max (2d-point-x a) (2d-point-x b)))
	  (max-y (max (2d-point-y a) (2d-point-y b))))
      (make-line :a (2dp min-x min-y) :b (2dp max-x max-y)))))

(defmethod bounding-box ((bezier bezier))
  ;; billig founctionniert nicht XXX
  (with-slots (a b) bezier
    (let ((min-x (min (2d-point-x a) (2d-point-x b)))
	  (min-y (min (2d-point-y a) (2d-point-y b)))
	  (max-x (max (2d-point-x a) (2d-point-x b)))
	  (max-y (max (2d-point-y a) (2d-point-y b))))
      (make-line :a (2dp min-x min-y) :b (2dp max-x max-y)))))

(defmethod bounding-box ((l list))
  (let* ((bboxes (mapcar #'bounding-box l))
	 (min-x (reduce #'min (mapcar #'(lambda (x) (2d-point-x (line-a x))) bboxes)))
	 (min-y (reduce #'min (mapcar #'(lambda (x) (2d-point-y (line-a x))) bboxes)))
	 (max-x (reduce #'max (mapcar #'(lambda (x) (2d-point-x (line-b x))) bboxes)))
	 (max-y (reduce #'max (mapcar #'(lambda (x) (2d-point-y (line-b x))) bboxes))))
    (make-line :a (2dp min-x min-y) :b (2dp max-x max-y))))

(defun bbox-below-p (obj y)
  (let ((bbox (bounding-box obj)))
    (< (2d-point-y (line-b bbox)) y)))

(defun bbox-above-p (obj y)
  (let ((bbox (bounding-box y)))
    (> (2d-point-y (line-a bbox)) y)))

(defun bbox-width (bbox)
  (- (2d-point-x (line-b bbox))
     (2d-point-x (line-a bbox))))

(defun bbox-height (bbox)
  (- (2d-point-y (line-b bbox))
     (2d-point-y (line-a bbox))))

(defun bbox-bottom (bbox)
  (2d-point-y (line-a bbox)))

(defun bbox-top (bbox)
  (2d-point-y (line-b bbox)))

(defun bbox-left (bbox)
  (2d-point-x (line-a bbox)))

(defun bbox-right (bbox)
  (2d-point-x (line-b bbox)))

(defparameter *plywood-board-tool*
  (make-instance 'tool
		 :diameter 1
		 :number 6
		 :feed-xy 600
		 :feed-z 240
		 :depth 2))


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
	    
	  (with-named-pass ("inner")
	    (with-tool (*plywood-board-tool*)
	      (mill-curve (offset-curve (first curves-outline) 3) :depth 0.7 :scale scale)))
	  
	  (with-named-pass ("outline")
	    (with-tool (*plywood-board-tool*)
	      (mill-curve (offset-curve (first curves-outline) -6) :scale scale :depth 4))))))))

;; board = 1035 pixel width
(defun test-eyes-offset (&key (scale 0.4))
  (with-program ("maennchen")
    (with-tool (*pcb-tool*)
  (let ((bbox (bounding-box (append *outline* *eyes*))))
    (with-tool (*pcb-tool*)
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
    (with-tool (*pcb-tool*)
      (spindle-on)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
      (maennchen-trace-panel num :scale scale :cache cache))))

(defun interpret-potrace (potrace)
  (with-program ("potrace")
    (with-tool (*pcb-tool*)
      (with-named-pass ("mill")
	(with-tool-down ()
	  (with-transform ((scaling-matrix 0.3))
	    (loop for curve in potrace
	       do (loop for i in curve
		     do (cond
			  ((typep i 'bezier)
			   (interpret-bezier-arcs i))
			  ((typep i 'line)
			   (let ((a (line-a i))
				 (b (line-b i)))
			     (mill-point a)
			     (mill-point b))))))))))))

