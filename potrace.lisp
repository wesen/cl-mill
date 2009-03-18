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

(defun mill-segment-ramp (seg to-z)
  (cond ((typep seg 'line)
	 (mill-abs :x (2d-point-x (line-b seg))
		   :y (2d-point-y (line-b seg))
		   :z to-z))
	((typep seg 'arc)
	 (with-slots (a b centre direction) seg
	   (cond ((eq direction :cw)
		  (arc-cw  :x (2d-point-x b) :y (2d-point-y b) :z to-z 
			   :i (2d-point-x centre) :j (2d-point-y centre)))
		 ((eq direction :ccw)
		  (arc-ccw :x (2d-point-x b) :y (2d-point-y b) :z to-z 
			   :i (2d-point-x centre) :j (2d-point-y centre))))))))


;; XXX add ramping



(defmacro repeat-for-depth-ramp (list depth &key (ramplen 10))
  (let ((curx (gensym))
	(cury (gensym)))
    `(let* ((nums (ceiling (/ ,depth (tool-depth *current-tool*))))
	    ramp-in ramp-out main)
       
       (if (circular-segment-p ,list)
	   (let* ((ramps (collect-segments-for-length ,list ,ramplen))
		  (ramps2 (collect-segments-for-length (second ramps) ,ramplen)))
	     (setf ramp-in (first ramps)
		   ramp-out (first ramps2)
		   main (second ramps)))
	   (let* ((ramps (collect-segments-for-length ,list ,ramplen))
		  (ramps2 (collect-segments-for-length (reverse (second ramps)) ,ramplen)))
	     (setf ramp-in (first ramps)
		   ramp-out (reverse (first ramps2))
		   main (reverse (second ramps2)))))

       ;;(format t "nums: ~A ramp in : ~A~%main: ~A~%ramp out: ~A~%" nums ramp-in main ramp-out)
       
	    (with-transform ((scaling-matrix scale))
	      
       (loop for i from 0 below nums
	  for curdepth from 0 by (tool-depth *current-tool*)
	  for nextdepth = (min ,depth (+ curdepth (tool-depth *current-tool*)))
	  do 
	    (let ((,curx (orig-current-x))
		  (,cury (orig-current-y)))

	      (loop for ramp-seg in ramp-in
		 with len = (object-length ramp-in)
		 with milled-len = 0
		 do
		   (incf milled-len (object-length ramp-seg))
		   (mill-segment-ramp ramp-seg (- (+ curdepth
						     (* (- nextdepth curdepth)
							(/ milled-len len)))))
		   (format t "IN -> ~A~%"
			   (+ curdepth
			      (* (- nextdepth curdepth)
				 (/ milled-len len)))))

	      (loop for i in main
		 do
		     (mill-segment i))

	      (format t "~A ~A~%" i (1- nums))
	      (when (= i (1- nums))
		(format t "z: ~A~%" (current-z))
		(loop for i in ramp-in
		     do (mill-segment i))
		
		(loop for ramp-seg in ramp-out
		   with len = (object-length ramp-out)
		   with milled-len = len
		   do
		     (decf milled-len (object-length ramp-seg))
		     (format t "milled-len: ~A, len: ~A~%" milled-len len)
		     (mill-segment-ramp ramp-seg (* (current-z)
						    (/ milled-len len)))
		     (format t "OUT -> ~A~%"
			     (* (current-z)
				(/ milled-len len)))))

	      (unless (or (= i (1- nums))
			  (and (epsilon-= ,curx (orig-current-x))
			       (epsilon-= ,cury (orig-current-y))))
		(tool-up)
		(goto-abs :x ,curx :y ,cury)
		(tool-down :depth curdepth))))

       (tool-up)))))
    
(defun collect-segments-for-length (segments length)
  (let ((res)
	(len 0))
    (do ((segs segments (cdr segs))
	 (col nil))
	((or (null segs)
	     (>= len length))
	 (list (nreverse col) segs))
      (incf len (object-length (first segs)))
      (push (first segs) col))))

(defun mill-curve (curve &key depth (scale 1) (ramp t))
  (let ((point (object-start-point (first curve))))
    (with-transform ((scaling-matrix scale))
      (tool-up)
      (goto :x (2d-point-x point) :y (2d-point-y point))))

  (if ramp
      (repeat-for-depth-ramp curve (or depth (tool-depth *current-tool*)))
      (repeat-for-depth ((or depth (tool-depth *current-tool*)))
			(with-transform ((scaling-matrix scale))
			  (dolist (seg curve)
			    (mill-segment seg))))))

(defun test-circle (radius)
  (list
   (make-arc :a (2dp (- radius) 0)
	     :b (2dp 0 radius)
	     :centre (2dp 0 0)
	     :direction :cw)
   (make-arc :a (2dp 0 radius)
	     :b (2dp radius 0)
	     :centre (2dp 0 0)
	     :direction :cw)
   (make-arc :a (2dp radius 0)
	     :b (2dp 0 (- radius))
	     :centre (2dp 0 0)
	     :direction :cw)
   (make-arc :a (2dp 0 (- radius))
	     :b (2dp (- radius) 0)
	     :centre (2dp 0 0)
	     :direction :cw)))

(defun interpret-curve (CurvE &key depth)
  (with-program ("potrace")
    (with-tool (*current-tool*)
      (with-named-pass ("mill")
	(mill-curve curve :depth depth)))))

(defun test-curve (curve)
  (with-program ("potrace")
    (with-tool (*trace-tool*)
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

(defmethod bounding-box ((arc arc))
  ;; XXX billig, stimmt nicht
  (with-slots (a b) arc
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
		 :diameter 2
		 :number 6
		 :feed-xy 600
		 :feed-z 240
		 :depth 2))


(defparameter *trace-tool*
  *plywood-board-tool*)


(defun interpret-potrace (potrace)
  (with-program ("potrace")
    (with-tool (*trace-tool*)
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

