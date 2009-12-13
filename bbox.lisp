(in-package :gcode)

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

(defun rotate-and-bring-to-zero (object angle)
  (let* ((robj (transform-object object (rotation-matrix angle)))
	 (bbox (bounding-box robj))
	 (bottom (bbox-bottom bbox))
	 (left (bbox-left bbox)))
    (transform-object robj (translation-matrix (- left) (- bottom)))))

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