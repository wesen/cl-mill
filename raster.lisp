(in-package :gcode)

(defun make-raster-line (y)
  (make-long-line :a (2dp 0 y) :b (2dp 1 y)))

(defun line-intersections (line curve)
  (let ((*epsilon* 0.1))
;;    (format t "~%")
    (loop for seg in curve
	 for intersects = (intersection-object line seg)
	 appending intersects
;;	 unless (null intersects)
;;       do ;;(format t "~A~% intersects with seg ~A =>~% ~A~%" line seg intersects)
	 )))

(defun intersecting-y (curve y)
  (loop for seg in curve
       for start = (object-start-point seg)
       for end = (object-end-point seg)
       when (or (and (<= y (2d-point-y start))
		     (>= y (2d-point-y end)))
		(and (>= y (2d-point-y start))
		     (<= y (2d-point-y end))))
       collect seg))

(defun nearest-seg-point (seg y)
  (let* ((start (object-start-point seg))
	 (end (object-end-point seg)))
    (if (> (abs (- (2d-point-y start) y))
	   (abs (- (2d-point-y end) y)))
	end
	start)))

(defun intersecting-points-line (curve y)
  (let ((segs (intersecting-y curve y))
	(line (make-raster-line y)))
    (loop for seg in segs
	 for intersects = (intersection-object line seg)
	 for nearest = (nearest-seg-point seg y)
;;	 do (format t "y: ~A segment: ~A~% interse: ~A~%nearest: ~A~%" y seg intersects (nearest-seg-point seg y))
	 appending (if (or (null intersects)
			   (< (abs (- (2d-point-y nearest) y))
			      (abs (- (2d-point-y (first intersects)) y))))
		       (list (nearest-seg-point seg y))
		       intersects))))


(defgeneric extend-seg (obj step))

(defmethod extend-seg ((l1 line) step)
  (let ((norm (normalize-vector l1)))
    (with-slots (a b) l1
      (make-line :a (point-+ a (point-* norm step))
		 :b (point-+ b (point-* norm (- step)))))))

(defmethod extend-seg ((a1 arc) step)
  a1)

(defun remove-points-clever (points)
  (when (oddp (length points))
    (let ((p2 (remove-duplicates points :test #'point-=)))
      (if (evenp (length p2))
	  (return-from remove-points-clever p2))))
  (when (oddp (length points))
    (dolist (*epsilon* '(1 0.1 0.001 0.001 0.0001))
      (let ((p2 (remove-duplicates points :test #'point-epsilon=)))
	(if (evenp (length p2))
	    (return-from remove-points-clever p2)))))
  points)

(defun intersections-to-segments (intersections y)
  (let* ((*epsilon* 0.001)
	 (sorted-points (sort intersections #'< :key #'2d-point-x)))
;;    (format t "intersections: ~A~%" sorted-points)
    (setf sorted-points (remove-points-clever sorted-points))
;;    (format t "sorted points: ~A~%" sorted-points)
;;    (format t "length: ~A~%" (length sorted-points))
    (loop for (a b) on sorted-points by #'cddr
	 unless (null b)
	 collect (make-line :a (2dp (2d-point-x a) y) :b (2dp (2d-point-x b) y)))))

(defun sdl-draw-raster (curves &key (start-y 0) (end-y 300) (step 1))
  (sdl:with-init ()
    (sdl:window 1240 600)
    (setf (sdl:frame-rate) -1)
    (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))

    (dolist (curve curves)
      (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	(draw-curve (curve-to-arcs curve) :color col))
      (sdl:with-color (col (sdl:color :r 255 :g 0 :b 255))
	(dolist (seg (raster curve :start-y start-y :end-y end-y :step step))
	  (draw-curve (list seg) :color col))))
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t))))

(defvar *current-curve* nil)

(defun raster (curve &key (start-y 0) (end-y 300) (step 1))
  (let (res
	(*current-curve* curve))
    (raster-tsp-nearest-neighbor
     (loop for y from start-y below end-y by step
	appending (intersections-to-segments (intersecting-points-line curve y) y)))))

(defmethod cnc-distance ((p1 2d-point) (p2 2d-point))
  (let* ((line (make-line :a p1 :b p2))
	 (intersections #+nil(mapcan #'(lambda (x) (intersection-object line x))
				*current-curve*)))
    (setf intersections (remove p1 (remove p2 intersections :test #'point-epsilon=)
				:test #'point-epsilon=))
;;    (format t "cnc distance ~A ~A : ~A~%" p1 p2 intersections)
    (if (null intersections)
	(max (abs (- (2d-point-x p1) (2d-point-x p2)))
	     (abs (- (2d-point-y p1) (2d-point-y p2))))
	MOST-POSITIVE-FIXNUM)))

(defmethod cnc-distance ((p 2d-point) (l line))
  (min (cnc-distance p (line-a l))
       (cnc-distance p (line-b l))))

(defun raster-tsp-nearest-neighbor (segments &key (start (2dp 0 0)))
  (loop with current-segment = (nearest-neighbor start segments)
       with old-segment = nil
       with current-point = (line-b current-segment)
       with rest-segments = (remove current-segment segments)
       for line = (unless (null old-segment)
		    (make-line :a (line-b old-segment) :b (line-a current-segment)))
;;       do
;;       (format t "old ~A~%current ~A~%point ~A~%" old-segment current-segment current-point)
     unless (or (null old-segment)
		(> (line-length line) 5)
		(not (null (line-intersections line *current-curve*))))
       ;; check crossing of mill path, maybe move later to milling phase
     collect line

       ;; XXX up in the air
       
;;     unless (or (null old-segment)
;;		(> (line-length (make-line :a (line-b old-segment) :b (line-a current-segment))) 5))
;;       do (format t "connecting~%")
;;     else do  (format t "not connecting~%")
       
     collect current-segment
     until (null rest-segments)

     do (setf old-segment current-segment
	      current-segment (nearest-neighbor current-point rest-segments)
	      rest-segments (remove current-segment rest-segments :test #'line-=))
       (setf current-segment (reorder-line current-point current-segment)
	     current-point (line-b current-segment))))


(defun reorder-line (p l1)
  (with-slots (a b) l1
    (if (< (line-length (make-line :a p :b a))
	   (line-length (make-line :a p :b b)))
	l1
	(make-line :a b :b a))))

(defmethod nearest-neighbor ((p 2d-point) segments)
  (let ((segment-distances (loop for m2 in segments
			       collect (list (cnc-distance p m2) m2))))
    (let ((line (second (first (sort segment-distances #'< :key #'first)))))
      line)))

