(in-package :gcode)

(defun movement-move-p (move)
  (and (listp move)
       (member (first move) '(:g01 :g00 :g02 :g03))))

(defun movement-move-coord (move coord)
  (loop for i in move
     when (and (listp i)
	       (eql (first i) coord))
     do (return (second i))))

(defun movement-move-x (move)
  (movement-move-coord move :x))
(defun movement-move-y (move)
  (movement-move-coord move :y))
(defun movement-move-z (move)
  (movement-move-coord move :z))

(defun z-cross-p (old-z new-z)
  (not (= (sign old-z)
	  (sign new-z))))

(defun z-cross-move-p (old-z move)
  (and (movement-move-p move)
       (let ((z (movement-move-z move)))
	 (when z
	   (z-cross-p old-z z)))))

(defun z-part (z)
  (if (> z 0)
      :float
      :material))

(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)
   (z :initarg :z :accessor point-z)))

(defun point (x y z)
  (make-instance 'point :x x :y y :z z))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (with-slots (x y z) point
      (format stream "(~A, ~A, ~A)" x y z))))

(defmethod point-distance ((p1 point) (p2 point))
  (sqrt (+ (square (- (point-x p1) (point-x p2)))
	   (square (- (point-y p1) (point-y p2))))))

(defmethod rand-point-distance ((p1 point) (p2 point))
  (+ (point-distance p1 p2)
     (random 0.5)))

(defmethod cnc-distance ((p1 point) (p2 point))
  (max (abs (- (point-x p1) (point-x p2)))
       (abs (- (point-y p1) (point-y p2)))))

(defclass movement ()
  ((start :initarg :start :reader movement-start)
   (end :initarg :end :reader movement-end)
   (moves :Initarg :moves :reader movement-moves)))

(defmethod print-object ((movement movement) stream)
  (print-unreadable-object (movement stream :type t)
    (with-slots (start end moves) movement
      (format stream "(~A, ~A, ~A) -> (~A, ~A, ~A) ~A moves"
	      (point-x start) (point-y start) (point-z start)
	      (point-x end) (point-y end) (point-z end)
	      (length moves)))))

(defmethod cnc-distance ((m1 movement) (m2 movement))
  (cnc-distance (movement-end m1) (movement-start m2)))

;; arc length ?? leave out for now
(defun travel-length-moves (moves &key (start-z 50) (start-x 0) (start-y 0))
  (let ((current-x start-x)
	(current-y start-y)
	(current-z start-z)
	(length 0))
    (loop for move in moves
	 for old-x = current-x
	 for old-y = current-y
	 for old-z = current-z

       when (movement-move-p move)
       do (let ((x (movement-move-x move))
		(y (movement-move-y move))
		(z (movement-move-z move)))
	    (when x
	      (setf current-x x))
	    (when y
	      (setf current-y y))
	    (when z
	      (setf current-z z)))
	 (incf length (cnc-distance (point old-x old-y old-z)
				    (point current-x current-y current-z))))
    length))

(defun partition-z (moves &key (start-z 50) (start-x 0) (start-y 0))
  (let ((current-z start-z)
	(current-x start-x)
	(current-y start-y)
	(partitions)
	(current-partition (list)))
    (loop for move in moves

       when (movement-move-p move)
       do (let ((x (movement-move-x move))
		  (y (movement-move-y move))
		(z (movement-move-z move)))
	    (when x
	      (setf current-x x))
	    (when y
	      (setf current-y y))
	    (when z
	      (setf current-z z)))

       if (z-cross-move-p start-z move)
       do (let ((z (movement-move-z move)))
	    (push (make-instance 'movement
				 :start (point start-x start-y start-z)
				 :end (point current-x current-y current-z)
				 :moves (nreverse current-partition))
		  partitions)
	    
	    (setf start-z current-z
		  start-x current-x
		  start-y current-y
		  current-partition (list move)))
       else
       do (push move current-partition)

       finally (push (make-instance 'movement
				 :start (point start-x start-y start-z)
				 :end (point current-x current-y current-z)
				 :moves (nreverse current-partition))
		  partitions)
	 (return (nreverse partitions)))))

(defun optimize-movements (moves &key start)
  (tsp-nearest-neighbor (mill-moves moves) :start start))

(defun optimize-path (moves)
  (connect-movements (optimize-movements (partition-z moves))))

(defun mill-moves (moves)
  (remove-if-not #'(lambda (x) (<= 0 (point-z (movement-end x))))
		 moves))

(defmethod nearest-neighbor ((m movement) movements)
  (let ((movement-distances (loop for m2 in movements
			       collect (list (cnc-distance m m2) m2))))
    (second (first (sort movement-distances #'< :key #'first)))))

(defun random-elt (list)
  (elt list (random (length list))))

(defun tsp-nearest-neighbor (movements &key (start (point 0 0 0 )))
  (loop with current-movement = (nearest-neighbor
				 (make-instance 'movement :start (point 0 0 0)
						:end (point 0 0 0)
						:moves nil)
				 movements)
       with rest-movements = (remove current-movement movements)
       collect current-movement
       until (null rest-movements)
       do (setf current-movement (nearest-neighbor current-movement rest-movements)
		rest-movements (remove current-movement rest-movements))))

(defun connect-movements (movements &key (height *fly-height*))
  (let ((result))
      (loop for movement in movements
	   for start = (movement-start movement)
	 do (push `(:g00 :x ,(point-x start) :y ,(point-y start) :f
			 ,(tool-feed-xy *current-tool*))
		  result)
	   (loop for move in (movement-moves movement)
		do (push move result))
	   (push `(:g00 :z ,height :f ,(tool-feed-z *current-tool*))
		 result))
      (nreverse result)))

(defmethod optimize-program-pass (program name)
  (let ((pass (program-pass program name)))
    (setf (pass-moves pass)
	  (optimize-path (pass-moves pass)))))

(defun optimize-current-pass ()
  (optimize-program-pass *current-program* (pass-name *current-pass*)))

(defun optimize-pass (name)
  (optimize-program-pass *current-program* name))