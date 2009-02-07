(in-package :gcode)


;; general helper functions

(defun mm-per-sec-to-mm-per-minute (mm-per-sec)
  (* 60 mm-per-sec))

(defun mm-per-minute-to-mm-per-sec (mm-per-minute)
  (/ mm-per-minute 60.0))

(defun realize-number (x)
  (let ((num (if (numberp x)
		 (coerce x 'float)
		 x)))
    (if (< (abs num) 0.001)
      0
      num)))

(defun sign (number)
  (if (< number 0)
      -1
      +1))

(defun null-offset-p (x)
  (or (null x)
      (= x 0)))

;; trigonometry matrix stuff for transformations

(defparameter *PI* 3.141592653589793238462643)

(defparameter *unity-matrix*
  (let ((res (make-array '(3 3) :initial-element 0)))
    (setf (aref res 0 0) 1
	  (aref res 1 1) 1
	  (aref res 2 2) 1)
    res))

(defun deg-to-radians (angle)
  (* (/ angle 360.0) 2 *PI*))

(defun radians-to-deg (angle)
  (* 360.0 (/ angle (* 2 *PI*))))

(defun rotation-matrix (angle)
  (let ((res (make-array '(3 3) :initial-element 0)))
    (setf (aref res 0 0) (cos (deg-to-radians angle))
	  (aref res 0 1) (sin (deg-to-radians angle))
	  (aref res 1 0) (- (sin (deg-to-radians angle)))
	  (aref res 1 1) (cos (deg-to-radians angle))
	  (aref res 2 2) 1)
    res))

(defun translation-matrix (x y)
  (let ((res (copy-matrix *unity-matrix*)))
    (setf (aref res 0 2) x
	  (aref res 1 2) y)
    res))

(defun matrix-multiply (a b)
  (let ((res (make-array '(3 3) :initial-element 0)))
    (dotimes (i 3)
      (dotimes (j 3)
	(dotimes (k 3)
	  (incf (aref res i j)
		(* (aref a i k)
		   (aref b k j))))))
    res))

(defun copy-matrix (matrix)
  (let ((res (make-array (array-dimensions matrix))))
    (dotimes (i (array-dimension matrix 0))
      (dotimes (j (array-dimension matrix 1))
	(setf (aref res i j)
	      (aref matrix i j))))
    res))

(defun invert-matrix (matrix)
  (let* ((a (aref matrix 0 0))
	 (b (aref matrix 0 1))
	 (c (aref matrix 0 2))
	 (d (aref matrix 1 0))
	 (e (aref matrix 1 1))
	 (f (aref matrix 1 2))
	 (g (aref matrix 2 0))
	 (h (aref matrix 2 1))
	 (i (aref matrix 2 2))

	 (factor (/ 1 (+ (* a (- (* e i) (* f h)))
			 (- (* b (- (* d i) (* f g))))
			 (* c (- (* d h) (* e g)))))))
    
    (let ((res (make-array '(3 3))))
      (setf (aref res 0 0)
	    (* factor (- (* e i) (* f h))))
      (setf (aref res 0 1)
	    (* factor (- (* c h) (* b i))))
      (setf (aref res 0 2)
	    (* factor (- (* b f) (* c e))))
      (setf (aref res 1 0)
	    (* factor (- (* f g) (* d i))))
      (setf (aref res 1 1)
	    (* factor (- (* a i) (* c g))))
      (setf (aref res 1 2)
	    (* factor (- (* c d) (* a f))))
      (setf (aref res 2 0)
	    (* factor (- (* d h) (* e g))))
      (setf (aref res 2 1)
	    (* factor (- (* b g) (* a h))))
      (setf (aref res 2 2)
	    (* factor (- (* a e) (* b d))))
      res)))

(defun scaling-matrix (scale)
  (let ((res (copy-matrix *unity-matrix*)))
    (setf (aref res 0 0) scale
	  (aref res 1 1) scale)
    res))

(defun vector-angle (x1 y1 x2 y2)
  (let ((xdiff (- x2 x1))
	(ydiff (- y2 y1)))
    (radians-to-deg
     (- (atan ydiff xdiff)
	(atan 0 1)))))

(defun vector-length (x1 y1 x2 y2)
  (let ((xdiff (- x2 x1))
	(ydiff (- y2 y1)))
    (sqrt (+ (* xdiff xdiff)
	     (* ydiff ydiff)))))
	
(defun rotate-vector (x y matrix)
  (list (+ (* x (aref matrix 0 0))
	   (* y (aref matrix 0 1)))
	(+ (* x (aref matrix 1 0))
	   (* y (aref matrix 1 1)))))

(defun transform-vector (x y matrix)
  (list (+ (* x (aref matrix 0 0))
	   (* y (aref matrix 0 1))
	   (aref matrix 0 2))
	(+ (* x (aref matrix 1 0))
	   (* y (aref matrix 1 1))
	   (aref matrix 1 2))))


(defparameter *epsilon* 0.01)

(defun epsilon-= (a b &key (epsilon *epsilon*))
  (< (abs (- a b)) epsilon))

(defvar *current-transform* *unity-matrix*)

(defmacro with-transform ((transform) &rest body)
  `(let ((*current-transform* (matrix-multiply *current-transform* ,transform)))
     ,@body))

