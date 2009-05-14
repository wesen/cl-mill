(in-package :gcode)
(defstruct 2d-point
  x y)

(defstruct line
  a b)

(defstruct circle
  centre radius)

(defun square (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun 2-point-length (p1 p2)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2)))
    (sqrt (+ (square (- x1 x2))
	     (square (- y1 y2))))))

(defun line-length (s1)
  (2-point-length (line-a s1) (line-b s1)))

(defun incenter (p1 p2 p3)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2))
	(x3 (2d-point-x p3))
	(y3 (2d-point-y p3)))
    (let ((l2 (2-point-length p1 p3))
	  (l1 (2-point-length p2 p3))
	  (l3 (2-point-length p1 p2)))
      (make-2d-point :x (/ (+ (* l1 x1) (* l2 x2) (* l3 x3))
			   (+ l1 l2 l3))
		     :y (/ (+ (* l1 y1) (* l2 y2) (* l3 y3))
			   (+ l1 l2 l3))))))
  
;; line going through 2 points into slope-intercept orm y = a * x + b
(defun line-equation (p1 p2)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2)))
    (let* ((a (/ (- y2 y1) (- x2 x1)))
	   (b (- y1 (* a x1))))
      (list a b))))

;; distance of x3,y3 to line going through x1,y1 and x2,y2
(defun point-line-distance (p3 p1 p2)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2))
	(x3 (2d-point-x p3))
	(y3 (2d-point-y p3)))
    (let* ((u (/ (+ (* (- x3 x1) (- x2 x1))
		    (* (- y3 y1) (- y2 y1)))
		 (+ (square (- x1 x2))
		    (square (- y1 y2)))))
	   (x (+ x1 (* u (- x2 x1))))
	   (y (+ y1 (* u (- y2 y1)))))
      (2-point-length (make-2d-point :x x :y y) p3))))

;; intersection of two lines given by x1,y1 - x2 y2 and x3, y3 - x4, y4
(defun line-intersection (p1 p2 p3 p4)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2))
	(x3 (2d-point-x p3))
	(y3 (2d-point-y p3))
	(x4 (2d-point-x p4))
	(y4 (2d-point-y p4)))
    (let* ((num (- (* (- x4 x3) (- y1 y3))
		   (* (- y4 y3) (- x1 x3))))
	   (denum (- (* (- y4 y3) (- x2 x1))
		     (* (- x4 x3) (- y2 y1))))
	   (ua (unless (= denum 0)
		 (/ num denum))))
;;      (format t "num: ~A denum: ~A ua: ~A~%" num denum ua)
      (cond ((and (= num 0) (= denum 0))
	     :parallel)
	    ((= denum 0)
	     nil)
	    (t
	     (make-2d-point :x (+ x1 (* ua (- x2 x1)))
			    :y (+ y1 (* ua (- y2 y1)))))))))
    
(deftest :intersection "line intersection"
  (intersection-line (make-line :a (2dp 105 105) :b (2dp 105 108))
		     (make-line :a (2dp 105 108) :b (2dp 105 112))))

(defun intersection-line (l1 l2)
  (line-intersection (line-a l1) (line-b l1) (line-a l2) (line-b l2)))

(defun on-segment-p (l1 p)
  (epsilon-= (line-length l1)
     (+ (line-length (make-line :a p :b (line-a l1)))
	(line-length (make-line :a p :b (line-b l1))))))

(defun test-on-segment-p ()
  (let ((l1 (make-line :a (2dp 0 0) :b (2dp 10 0)))
	(p1 (2dp 1 0))
	(p2 (2dp 10 0))
	(p3 (2dp 0 0))
	(p4 (2dp -1 0))
	(p5 (2dp 0 1))
	(p6 (2dp 11 0)))
    (dolist (p (list p1 p2 p3 p4 p5 p6))
      (let ((onseg (on-segment-p l1 p)))
	(format t "onseg ~A ~A: ~A~%" l1 p onseg)))))

(defun 2dp (x y)
  (make-2d-point :x x :y y))

(defmacro swap (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setf ,a ,b
	     ,b ,tmp))))
	

(defun circle-through-3-points (p1 p2 p3)
  (when (or (= (- (2d-point-x p1) (2d-point-x p2)) 0)
	    (= (- (2d-point-x p2) (2d-point-x p3)) 0))
    #+nil(format t "swap~%")
    (swap p2 p3))
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2))
	(x3 (2d-point-x p3))
	(y3 (2d-point-y p3)))
    #+nil(format t "x1: ~A y1: ~A~%x2: ~A y2: ~A~%x3: ~A y3: ~A~%"
	    x1 y1 x2 y2 x3 y3)
    (let* ((ma (/ (- y2 y1) (- x2 x1)))
	   (mb (/ (- y3 y2) (- x3 x2)))
	   (cx (/ (+ (* ma mb (- y1 y3))
		     (* mb (+ x1 x2))
		     (- (* ma (+ x2 x3))))
		  (* 2 (- mb ma))))
	   (cy (if (= mb 0)
		   (+ (* (- (/ 1 ma)) (- cx (/ (+ x1 x2) 2)))
		      (/ (+ y1 y2) 2))
		   (+ (* (- (/ 1 mb)) (- cx (/ (+ x2 x3) 2)))
		      (/ (+ y2 y3) 2)))))
      (make-2d-point :x cx :y cy))))

(defun interpolate-line (p1 p2 time)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2)))
    (make-2d-point :x (+ x1 (* time (- x2 x1)))
		   :y (+ y1 (* time (- y2 y1))))))

(defun bezier-biarc (bezier)
  (with-slots (a u v b) bezier
    (let* ((inter (line-intersection a u b v))
	   (g (when inter (incenter a inter b))))
      (when (null inter)
	(warn "no intersection of bezier control lines: ~A~%" bezier))
      (when g (circle-through-3-points a b g)))))

(defun bezier-biarc-angle (bezier)
  (with-slots (a b) bezier
    (let ((center (bezier-biarc bezier)))
;;      (format t "center: ~A~%" center)
      (radians-to-deg (angle-2-segments (make-line :a center :b a)
					(make-line :a center :b b))))))

(defun test-biarc ()
  (let ((b1 (make-bezier :a (2dp 2 2) :u (2dp 1 5) :b (2dp 6 5) :v (2dp 4 7))))
    (format t "~A angle: ~A~%" b1 (bezier-biarc-angle b1))))

(defun dot-product (s1 s2)
  (+ (* (2d-point-x s1) (2d-point-x s2))
     (* (2d-point-y s1) (2d-point-y s2))))

(defun point-- (p1 p2)
  (2dp (- (2d-point-x p1) (2d-point-x p2))
       (- (2d-point-y p1) (2d-point-y p2))))

(defun point-+ (p1 p2)
  (2dp (+ (2d-point-x p1) (2d-point-x p2))
       (+ (2d-point-y p1) (2d-point-y p2))))

(defun vector-- (s1 s2)
  (let ((a1 (line-a s1))
	(b1 (line-b s1))
	(a2 (line-a s2))
	(b2 (line-b s2)))
    (make-line :a (point-- a1 a2)
	       :b (point-- b1 b2))))

(defun vector-+ (s1 s2)
  (let ((a1 (line-a s1))
	(b1 (line-b s1))
	(a2 (line-a s2))
	(b2 (line-b s2)))
    (make-line :a (point-+ a1 a2)
	       :b (point-+ b1 b2))))

(defun point-/ (p a)
  (2dp (/ (2d-point-x p) a)
       (/ (2d-point-y p) a)))

(defun point-* (p a)
  (2dp (* (2d-point-x p) a)
       (* (2d-point-y p) a)))

(defun normalize-vector (s1)
  (with-slots (a b) s1
    (point-/ (point-- b a) (line-length s1))))

(defun angle-2-segments (s1 s2)
  (acos (dot-product (normalize-vector s1)
		     (normalize-vector s2))))

(defun angle-2-segments-directed (s1 s2)
  ;; winkel < 180 > -180
  (let ((ns1 (normalize-vector s1))
	(ns2 (normalize-vector s2)))
    (let ((res (- (atan (2d-point-y ns2) (2d-point-x ns2))
		  (atan (2d-point-y ns1) (2d-point-x ns1)))))
      res)))

(defun interpolate-2-points (p1 p2 time)
  (let ((x1 (2d-point-x p1))
	(y1 (2d-point-y p1))
	(x2 (2d-point-x p2))
	(y2 (2d-point-y p2)))
    (2dp (+ x1 (* time (- x2 x1)))
	 (+ y1 (* time (- y2 y1))))))


;;;

(defun intersection-line-circle (p1 p2 c radius)
  (let* ((_p1 (point-- p1 c))
	 (_p2 (point-- p2 c))
	 (x1 (2d-point-x _p1))
	 (y1 (2d-point-y _p1))
	 (x2 (2d-point-x _p2))
	 (y2 (2d-point-y _p2))
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (dr (sqrt (+ (square dx) (square dy))))
	 (d  (!! x1 * y2 - x2 * y1))
	 (dis (!! (square radius)  * (square dr) - (square d))))
    (cond ((> dis 0)
	   (let ((incx (!! (sign dy) * dx * (sqrt dis)))
		 (incy (!! (abs dy) * (sqrt dis))))
	     (list (point-+ c (2dp (!! (d * dy + incx) / (square dr))
				   (!! ((- d) * dx + incy) / (square dr))))
		   (point-+ c (2dp (!! (d * dy - incx) / (square dr))
				   (!! ((- d) * dx - incy) / (square dr)))))))
	  ((= dis 0)
	   (list (point-+ c (2dp (!! (d * dy) / (square dr))
				 (!! ((- d) * dx) / (square dr))))))
	  (t nil))))

(defun test-intersection-line-circle ()
  (let ((c (2dp 2 2))
	(radius 2))
    (format t "intersection: ~A~%"
	    (intersection-line-circle
	     (2dp -2 2) (2dp 6 2)
	     c radius))
    (format t "intersection: ~A~%"
	    (intersection-line-circle
	     (2dp 0 0) (2dp 4 4)
	     c radius))
    (format t "intersection: ~A~%"
	    (intersection-line-circle
	     (2dp 0 4) (2dp 4 4)
	     c radius))
    (format t "intersection: ~A~%"
	    (intersection-line-circle
	     (2dp 0 5) (2dp 4 5)
	     c radius))
    ))

(defun vector-normal (l1)
  (let ((norm (normalize-vector l1)))
    (2dp (- (2d-point-y norm)) (2d-point-x norm))))

(defun line-point-side (l1 p1)
  (case (triangle-ccw p1 (line-a l1) (line-b l1))
    (1 :left)
    (-1 :right)
    (0 :on)))

(defun triangle-ccw (p1 p2 p3)
  (if (or (point-= p1 p2)
	  (point-= p1 p3)
	  (point-= p2 p3))
      0
      (let* ((p1x (2d-point-x p1))
	     (p1y (2d-point-y p1))
	     (p2x (2d-point-x p2))
	     (p2y (2d-point-y p2))
	     (p3x (2d-point-x p3))
	     (p3y (2d-point-y p3))
	     (dx1 (- p2x p1x))
	     (dy1 (- p2y p1y))
	     (dx2 (- p3x p1x))
	     (dy2 (- p3y p1y)))
	(cond ((> (* dx1 dy2) (* dy1 dx2))
	       1)
	      ((< (* dx1 dy2) (* dy1 dx2))
	       -1)
	      ((or (< (* dx1 dx2) 0)
		   (< (* dy1 dy2) 0))
	       -1)
	      ((< (+ (square dx1) (square dy1))
		  (+ (square dx2) (square dy2)))
	       1)
	      (t 0)))))

(defun line-line-side (l1 l2)
  ;; l2 side of l1
  (let ((c1 (line-point-side l1 (line-a l2)))
	(c2 (line-point-side l1 (line-b l2))))
    (cond ((eq c1 c2)
	   c1)
	  ((eq c1 :on)
	   c2)
	  ((eq c2 :on)
	   c1)
	  (t :cross))))

(defgeneric object-start-point (object))
(defgeneric object-end-point (object))

(defmethod object-start-point ((line line))
  (line-a line))

(defmethod object-end-point ((line line))
  (line-b line))

(deftest :line-kaputt "line kaputt"
  (line-intersection (2dp 449.1148052956802d0 155.99999999999997d0)
		     (make-2D-POINT :X 407.3874032937638d0 :Y 155.99999999999997d0)

		     (make-2D-POINT :X 101.80000000000291d0 :Y 24.999999999999943d0)
		      (make-2D-POINT :X 543.0d0 :Y 24.999999999999943d0)))

(defgeneric object-length (object))
(defmethod object-length ((line line))
  (line-length line))

(defmethod object-length ((list list))
  (reduce #'+ (mapcar #'object-length list)))