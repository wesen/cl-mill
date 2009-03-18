(in-package :gcode)

(defparameter *resolution* 15)
(defvar *round-steps* nil)
(defvar *cut-steps* nil)

(defun mill-rounded-edge (x1 y1 x2 y2 depth tool-diameter diameter &key (overlap 2))
  (let ((length (vector-length x1 y1 x2 y2))
	(angle (vector-angle x1 y1 x2 y2)))
    (with-transform ((translation-matrix x1 y1))
      (with-transform ((rotation-matrix (- 360 angle)))
	#+nil(mill-rounded-edge-normal length depth tool-diameter diameter
				       :overlap overlap)

	(mill-rounded-edge-hack length tool-diameter)
	))))

(defun mill-rounded-edge-normal (length depth tool-diameter
				 diameter &key (overlap 2))
   #+nil(goto-abs :z 2) ;; tool up
   #+nil(goto-abs :x 0 :y 0)
   #+nil(with-tool-down (depth)
	  (mill-abs :x length :y 0))
   
   (let* ((radius (/ diameter 2))
	  (steps (ceiling (/ length
			     (/ tool-diameter overlap))))
	  (move-by-step (/ length steps)))
     
     #+nil
     (format t "steps: ~A, move-by-step: ~A~%" steps move-by-step)
     
     (mill-abs :z *fly-height*)
     (goto-abs :x 0 :y 0)
     
     (loop for step from 0 to steps
	for current-x = (* move-by-step step)
	collect (mill-abs :z 1)
	collect (goto-abs :x current-x
			  :y 0)
	collect (loop for i from 0 to 90 by *resolution*
		   collect (mill-abs
			    :x current-x
			    :y (- radius (* radius (sin (deg-to-radians i))))
			    :z (- (* radius (cos (deg-to-radians i))) radius))))
     #+nil(mill-abs :z 2)
     #+nil(goto-abs :x length :y 0)
     ))

(defun mill-rounded-edge-hack (length tool-diameter)
  (mill-abs :z *fly-height*)
  (goto-abs :x 0 :y (/ tool-diameter 2))
  (with-tool-down (0.2)
    (mill-abs :x length :y (/ tool-diameter 2)))
  (mill-abs :z *fly-height*))

;;; *shiver*

(defparameter *overlap* 2)
(defparameter *step-width* 3.8)

(defun bridge-width ()
  (/ *step-width* 1.5))

(defun bridge-cut-rel (x y &key depth)
  (let ((x1 (orig-current-x))
	(y1 (orig-current-y)))
    (unless depth
      (setf depth (- (tool-depth *current-tool*) 1.0)))
    (with-named-pass ("bridge-cut")
      (goto-abs :x x1 :y y1)
      (with-tool-down (depth)
	(mill-rel :x x :y y)))))

(defun bridge-cut-abs (x y &key depth)
  (let ((x1 (orig-current-x))
	(y1 (orig-current-y)))
    (unless depth
      (setf depth (- (tool-depth *current-tool*) 1.0)))
    (with-named-pass ("bridge-cut")
      (goto-abs :x x1 :y y1)
      (with-tool-down (depth)
	(mill-abs :x x :y y)))))
  

(defun mill-bridge-rel (&key (x 0) (y 0))
  (bridge-cut-rel x y :depth (bridge-width))
  (tool-up)
  (goto-rel :x x :y y)
  (tool-down :depth *step-width*))

(defun mill-r (&optional (width *step-width*))
  (mill-rel :x width))
(defun mill-r- (&optional (width *step-width*))
  (mill-rel :x (- width (/ *tool-diameter* 2))))
(defun mill-r-- (&optional (width *step-width*))
  (mill-rel :x (- width *tool-diameter*)))
(defun mill-r+ (&optional (width *step-width*))
  (mill-rel :x (+ width (/ *tool-diameter* 2))))
(defun mill-r++ (&optional (width *step-width*))
  (mill-rel :x (+ width *tool-diameter*)))
(defun mill-bridge-r++ (&optional (width *step-width*))
  (mill-bridge-rel :x (+ width *tool-diameter*)))
  
(defun mill-l (&optional (width *step-width*))
  (mill-rel :x (- width)))
(defun mill-l- (&optional (width *step-width*))
  (mill-rel :x (- (- width (/ *tool-diameter* 2)))))
(defun mill-l-- (&optional (width *step-width*))
  (mill-rel :x (- (- width *tool-diameter*))))
(defun mill-l+ (&optional (width *step-width*))
  (mill-rel :x (- (+ width (/ *tool-diameter* 2)))))
(defun mill-l++ (&optional (width *step-width*))
  (mill-rel :x (- (+ width *tool-diameter*))))
(defun mill-bridge-l++ (&optional (width *step-width*))
  (mill-bridge-rel :x (- (+ width *tool-diameter*))))
  
(defun mill-u (&optional (width *step-width*))
  (mill-rel :y width))
(defun mill-u- (&optional (width *step-width*))
  (mill-rel :y (- width (/ *tool-diameter* 2))))
(defun mill-u-- (&optional (width *step-width*))
  (mill-rel :y (- width *tool-diameter*)))
(defun mill-u+ (&optional (width *step-width*))
  (mill-rel :y (+ width (/ *tool-diameter* 2))))
(defun mill-u++ (&optional (width *step-width*))
  (mill-rel :y (+ width *tool-diameter*)))
(defun mill-bridge-u++ (&optional (width *step-width*))
  (mill-bridge-rel :y (+ width *tool-diameter*)))
  
(defun mill-d (&optional (width *step-width*))
  (mill-rel :y (- width)))
(defun mill-d- (&optional (width *step-width*))
  (mill-rel :y (- (- width (/ *tool-diameter* 2)))))
(defun mill-d-- (&optional (width *step-width*))
  (mill-rel :y (- (- width *tool-diameter*))))
(defun mill-d+ (&optional (width *step-width*))
  (mill-rel :y (- (+ width (/ *tool-diameter* 2)))))
(defun mill-d++ (&optional (width *step-width*))
  (mill-rel :y (- (+ width *tool-diameter*))))
(defun mill-bridge-d++ (&optional (width *step-width*))
  (mill-bridge-rel :y (- (+ width *tool-diameter*))))

(defun mill-round (&key (x 0) (y 0))
  (let ((x1 (orig-current-x))
	(y1 (orig-current-y)))
    (with-named-pass ("bridge-cut")
      (goto-abs :x x1 :y y1)
      (with-tool-down ((bridge-width))
	(mill-rel :x x :y y)))
    
    (tool-up)
    (goto-rel :x x :y y)
    (tool-down :depth *step-width*)))

(defun s-mill-round-r (&optional (width *step-width*))
  (let* ((x1 (orig-current-x))
	 (y1 (orig-current-y))
	 (new-x (+ x1 width)))
    (with-named-pass ("rounding")
      (mill-rounded-edge x1 y1 new-x y1
			 *step-width* *tool-diameter* *tool-diameter* :overlap *overlap*))
    (mill-r)))

(defun s-mill-round-l (&optional (width *step-width*))
  (let* ((x1 (orig-current-x))
	 (y1 (orig-current-y))
	 (new-x (- x1 width)))
    (with-named-pass ("rounding")
      (mill-rounded-edge x1 y1 new-x y1
			 *step-width* *tool-diameter* *tool-diameter* :overlap *overlap*))
    (mill-l)))

(defun s-mill-round-u (&optional (width *step-width*))
  (let* ((x1 (orig-current-x))
	 (y1 (orig-current-y))
	 (new-y (+ y1 width)))
    (with-named-pass ("rounding")
      (mill-rounded-edge x1 y1 x1 new-y
			 *step-width* *tool-diameter* *tool-diameter* :overlap *overlap*))
    (mill-u)))

(defun s-mill-round-d (&optional (width *step-width*))
  (let* ((x1 (orig-current-x))
	 (y1 (orig-current-y))
	 (new-y (- y1 width)))
    (with-named-pass ("rounding")
      (mill-rounded-edge x1 y1 x1 new-y
			 *step-width* *tool-diameter* *tool-diameter* :overlap *overlap*))
    (mill-d)))

