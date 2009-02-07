(in-package :gcode)

(defun rectangle (width height &key ccw)
  "Draws a rectangle of width WIDTH and height HEIGHT starting from current position."
  (let ((right (> width 0))
	(up (> height 0)))
    ;; check if we are clockwise or not
    (unless (eql right up)
      (setf ccw (not ccw))))
  (if ccw
      (progn (mill-rel :x width :y 0)
	     (mill-rel :x 0 :y height)
	     (mill-rel :x (- width) :y 0)
	     (mill-rel :x 0 :y (- height)))
      (progn (mill-rel :x 0 :y height)
	     (mill-rel :x width :y 0)
	     (mill-rel :x 0 :y (- height))
	     (mill-rel :x (- width) :y 0))))

(defun rectangle-fill (width height offset &key ccw)
  (when (or (> offset width)
	    (> offset height))
    (error "can't rectangle fill ~A x ~A with offset ~A" width height offset))
  (let ((right (> width 0))
	(up (> height 0)))
    (unless (eql right up)
      (setf ccw (not ccw))))
  (with-rel-back-xy ()
    (if ccw
	(spiral-width width height offset)
	(spiral-height width height offset))))

(defun spiral-width (width height offset)
  (loop until (or (= width 0)
		  (= height 0))
     do
       ;; (format t "x: ~A, y : ~A~%" (current-x) (current-x))
       (mill-rel :x width :y 0)
       (mill-rel :x 0 :y height)
       (setf width (max 0 (- width offset)))
       (setf height (max 0 (- height offset)))
       (mill-rel :x (- width) :y 0)
       (mill-rel :x 0 :y (- height))
       (setf width (max 0 (- width offset)))
       (setf height (max 0 (- height offset)))))

(defun spiral-height (width height offset)
  (loop until (or (= width 0)
		   (= height 0))
     do
       ;;(format t "x: ~A, y : ~A~%" (current-x) (current-x))
       (mill-rel :x 0 :y height)
       (mill-rel :x width :y 0)
       (setf height (max 0 (- height offset)))
       (setf width (max 0 (- width offset)))
       (mill-rel :x 0 :y (- height))
       (mill-rel :x (- width) :y 0)
       (setf height (max 0 (- height offset)))
       (setf width (max 0 (- width offset)))))

;; mills with going into object
(defun rectangle-outline (width height &key (depth (tool-depth *current-tool*))
			  ccw)
  (let ((offset (/ (tool-diameter *current-tool*) 2.0)))
	  
    (goto-rel :x (* -1 (sign width) offset)
	      :y (* -1 (sign height) offset))
    (repeat-for-depth  (depth)
       (rectangle (+ width offset) (+ height offset) :ccw ccw))))

(defun rectangle-inline (width height &key (depth (tool-depth *current-tool*)) ccw)
  (let ((offset (/ (tool-diameter *current-tool*) 2.0)))
	  
    (goto-rel :x (* 1 (sign width) offset)
	      :y (* 1 (sign height) offset))
    (repeat-for-depth (depth)
       (rectangle (- width (tool-diameter *current-tool*))
		  (- height (tool-diameter *current-tool*))
		  :ccw ccw))))

(defun rectangle-mill (width height &key depth ccw)
  (let ((offset (/ (tool-diameter *current-tool*) 2.0)))
    (goto-rel :x (* 1 (sign width) offset)
	      :y (* 1 (sign height) offset))
    (repeat-for-depth (depth)
      (rectangle-fill (- width (tool-diameter *current-tool*))
		      (- height (tool-diameter *current-tool*))
		      (tool-diameter *current-tool*) :ccw ccw))))


;; assume head is at 0, x = cx, y = cy - radius/2 (bottom tangent point)
(defun circle (radius &key ccw)
  (if ccw
      (progn
	(arc-ccw-rel :x radius :y radius :i 0 :j radius :f (tool-feed-z *current-tool*))
	(arc-ccw-rel :x (- radius) :y radius :i (- radius) :j 0)
	(arc-ccw-rel :x (- radius) :y (- radius) :i 0 :j (- radius))
	(arc-ccw-rel :x radius :y (- radius) :i radius :j 0))
      (progn
       (arc-cw-rel :x (- radius) :y radius :i 0 :j radius :f (tool-feed-z *current-tool*))
       (arc-cw-rel :x radius :y radius :i radius :j 0)
       (arc-cw-rel :x radius :y (- radius) :i 0 :j (- radius))
       (arc-cw-rel :x (- radius) :y (- radius) :i (- radius) :j 0))))

(defun p5-circle (x y width)
  (decf width (+ (tool-diameter *current-tool*) 5))
  (goto-abs :x x :y (- y (/ width 2)))
  (with-tool-down ()
    (circle (/ width 2))))


(defun drill (&key x y diameter (depth (tool-depth *current-tool*)))
  (when (< diameter (tool-diameter *current-tool*))
    (warn "Can not drill hole that is ~A big, resorting to ~A~%"
	  diameter (tool-diameter *current-tool*))
    (setf diameter (tool-diameter *current-tool*)))
  (format t "depth: ~A~%" depth)
  (let ((d (/ (- diameter (tool-diameter *current-tool*)) 2)))
    (goto-abs :x x :y (- y d))
    (if (= d 0)
	(progn
	  (tool-down :depth depth)
	  (tool-up))
	(repeat-for-depth (depth)
			  (circle d)))))

(defun circle-fill (radius offset &key ccw)
  (when (> offset radius)
    (error "Can't circle-fill radius ~A with offset ~A" radius offset))
  (with-rel-back-xy ()
    (circle-spiral radius offset :ccw ccw)))

(defun circle-spiral (radius offset &key ccw)
  (let* ((cur-y 0)
	 (cur-r radius))
    (loop for next-y = (min (- radius (/ offset 2))
			    (+ cur-y offset))
       for next-r = (max (/ offset 2)
			 (- cur-r offset))
       do (progn (circle cur-r :ccw ccw)
		 (mill-rel :y (- next-y cur-y)))
       until (= cur-r (/ offset 2))
       do (setf cur-y next-y cur-r next-r))))

(defun circle-inline (radius &key ccw depth)
  (let ((offset (/ (tool-diameter *current-tool*) 2.0)))
    (goto-rel :y offset)
    (repeat-for-depth (depth)
       (circle (- radius offset) :ccw ccw))))

(defun circle-outline (radius &key ccw depth)
  (let ((offset (/ (tool-diameter *current-tool*) 2.0)))
    (goto-rel :y (- offset))
    (repeat-for-depth (depth)
       (circle (+ radius offset) :ccw ccw))))

(defun circle-mill (radius &key (depth (tool-depth *current-tool*)) ccw)
  (let ((offset (/ (tool-diameter *current-tool*) 2.0)))
    (goto-rel :y offset)
    (repeat-for-depth (depth)
      (circle-fill (- radius offset)
		   (tool-diameter *current-tool*) :ccw ccw))))

