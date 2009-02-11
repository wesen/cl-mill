(in-package :gcode)

(defstruct bezier
  a u v b)

(defun eval-bezier (bezier time)
  (with-slots (a b u v) bezier
    (let ((ax (2d-point-x a))
	  (ay (2d-point-y a))
	  (bx (2d-point-x b))
	  (by (2d-point-y b))
	  (ux (2d-point-x u))
	  (uy (2d-point-y u))
	  (vx (2d-point-x v))
	  (vy (2d-point-y v)))
      (let ((m0 (cube (- 1 time)))
	    (m1 (* 3 time (square (- 1 time))))
	    (m2 (* 3 (square time) (- 1 time)))
	    (m3 (cube time)))
	(let ((x (+ (* m0 ax)
		    (* m1 ux)
		    (* m2 vx)
		    (* m3 bx)))
	      (y (+ (* m0 ay)
		    (* m1 uy)
		    (* m2 vy)
		    (* m3 by))))
	  (2dp x y))))))

(defun split-bezier (bezier time)
  (with-slots (a b u v) bezier
    (let* ((u1 (interpolate-2-points a u time))
	   (v2 (interpolate-2-points v b time))
	   (top (interpolate-2-points u v time))
	   (v1 (interpolate-2-points u1 top time))
	   (u2 (interpolate-2-points top v2 time))
	   (sect (interpolate-2-points v1 u2 time))
	   (eval (eval-bezier bezier time)))

      #+nil(format t "sect: ~A eval: ~A~%" sect eval)
     
      (list (make-bezier :a a :b sect :u u1 :v v1)
	    (make-bezier :a sect :b b :u u2 :v v2)))))

(defun split-bezier-times (b times)
  (let ((times (sort times #'<))
	res)
    (loop with last-time = 0
	 with last-bezier = b
       for time in times
       for time2 = (/ (- time last-time) (- 1 last-time))
       for split = (split-bezier last-bezier time2)
       do
;;	 (format t "time: ~A time2: ~A split: ~A~%" time time2 split)
	 (push (first split) res)
	 (setf last-time time2
	       last-bezier (second split))
       finally (push last-bezier res))
    (nreverse res)))

(defun bezier-print-biarc-angles (beziers)
  (loop for i in beziers
       for angle = (bezier-biarc-angle i)
       do (format t "bezier: ~A angle: ~A~%" i
		  (bezier-biarc-angle i))
       do (when (> angle 90)
	    (format t "NEW: ~%")
	    (let* ((new-beziers (split-bezier i 0.5)))
	      (bezier-print-biarc-angles new-beziers))
	    (format t "----:~%"))))

(defun bezier-arc-direction (a b centre)
  (let ((angle (radians-to-deg (angle-2-segments-directed (make-line :a centre :b a)
							  (make-line :a centre :b b)))))
    (cond ((< angle -90)
	   :ccw)
	  ((> angle 90)
	   :cw)
	  ((< angle 0)
	   :cw)
	  ((>= angle 0)
	   :ccw))))

(defun bezier-arc-a1 (bezier)
  (with-slots (a b) bezier
    (let* ((m (eval-bezier bezier 0.5))
	   (centre (circle-through-3-points a m b)))
      (if (and centre
	       #+sbcl
	       (sb-ext:float-nan-p (2d-point-x centre))
	       #-sbcl
	       nil)
	  (progn
	    (warn "NAN centre a1 ~A~%" bezier)
	    (make-line :a a :b b))
	  (make-arc :a a :b b :centre centre
		    :direction (bezier-arc-direction a b centre))))))

(defun bezier-arc-a2 (bezier)
  (with-slots (a b) bezier
    (let* ((centre (bezier-biarc bezier)))
;;      (format t "centre; ~A~%" centre)
      (if (and centre
	       #+sbcl
	       (sb-ext:float-nan-p (2d-point-x centre))
	       #-sbcl
	       nil)
	  (progn (warn "NAN centre ~A~%" bezier)
		 (make-line :a a :b b))
	  (if centre
	      (make-arc :a a :b b :centre centre
			:direction (bezier-arc-direction a b centre))
	      (bezier-arc-a1 bezier))))))

(defun bezier-arc (bezier)
  (let* ((line (make-line :a (bezier-a bezier)
			  :b (bezier-b bezier)))
	 (l (line-length line)))
    (if (or (< l 0.2)
	    (bezier-parallel-p bezier))
	line
	(let* ((arc (bezier-arc-a2 bezier))
	       (radius (when (arc-p arc)
			 (arc-radius arc))))
	  (if (and radius (> radius (* 1000 l)))
	      line
	      arc)))))

(defun bezier-subdivision-point-s1 (bezier)
  0.5)

(defun bezier-subdivision-point-s2 (bezier)
  ;; intersection
  0.5)

(defun split-tests ()
  (let ((b1 (make-bezier :a (2dp 0 0) :b (2dp 10 0)
			 :u (2dp 0 10) :v (2dp 10 10)))
	(b2 (make-bezier :a (2dp 0 0) :u (2dp 0 10)
			 :b (2dp 10 10) :v (2dp 10 0)))
	(b3 (make-bezier :a (2dp 0 0) :b (2dp 10 0)
			 :u (2dp 7 5) :v (2dp 3 5))))
    (format t "bezier: ~A: ~A~%" b1 (split-bezier b1 0.5))
    (format t "bezier: ~A: ~A~%" b1 (split-bezier-times b1 '(0.3 0.6)))

    (format t "bezier: ~A: ~A~%" b2 (split-bezier b2 0.5))

    (format t "bezier: ~A: ~A~%" b3 (split-bezier b3 0.5))

    ))

(defun derivative-cross-product (bezier time)
  (with-slots (a u v b) bezier
    (let ((ax (2d-point-x a))
	  (ay (2d-point-y a))
	  (bx (2d-point-x b))
	  (by (2d-point-y b))
	  (ux (2d-point-x u))
	  (uy (2d-point-y u))
	  (vx (2d-point-x v))
	  (vy (2d-point-y v)))
      (let* ((_ax (- ux ax))
	     (_ay (- uy ay))
	     (_bx (- vx ux _ax))
	     (_by (- vy uy _ay))
	     (_cx (- bx vx _ax (* 2 _bx)))
	     (_cy (- by vy _ay (* 2 _by))))
	(infpre:!! (_ax * _by - _ay * _bx + time * (_ax * _cy - _ay * _cx) +
			(square time) * (_bx * _cy - _by * _cx)))))))

(defun inflection-points (bezier)
  (with-slots (a u v b) bezier
    (let ((ax (2d-point-x a))
	  (ay (2d-point-y a))
	  (bx (2d-point-x b))
	  (by (2d-point-y b))
	  (ux (2d-point-x u))
	  (uy (2d-point-y u))
	  (vx (2d-point-x v))
	  (vy (2d-point-y v)))
      (let* ((_ax (- ux ax))
	     (_ay (- uy ay))
	     (_bx (- vx ux _ax))
	     (_by (- vy uy _ay))
	     (_cx (- bx vx _ax (* 2 _bx)))
	     (_cy (- by vy _ay (* 2 _by))))
	(flet ((cross (time)
		 (infpre:!! (_ax * _by - _ay * _bx + time * (_ax * _cy - _ay * _cx) +
				 (square time) * (_bx * _cy - _by * _cx)))))
	  (loop for time from 0 below 1 by 0.01
	       collect (list time (cross time))))))))

(defun bezier-interval-p (x)
  (and (> x 0)
       (< x 1)))


(defun solve-inflection-points (bezier)
  (remove-if-not #'bezier-interval-p
    (with-slots (a u v b) bezier
      (let ((ax (2d-point-x a))
	    (ay (2d-point-y a))
	    (bx (2d-point-x b))
	    (by (2d-point-y b))
	    (ux (2d-point-x u))
	    (uy (2d-point-y u))
	    (vx (2d-point-x v))
	    (vy (2d-point-y v)))
	(let* ((_ax (- ux ax))
	       (_ay (- uy ay))
	       (_bx (- vx ux _ax))
	       (_by (- vy uy _ay))
	       (_cx (- bx vx _ax (* 2 _bx)))
	       (_cy (- by vy _ay (* 2 _by)))
	       
	       (coeff-c (!! (_ax * _by - _ay * _bx)))
	       (coeff-b (!! (_ax * _cy - _ay * _cx)))
	       (coeff-a (!! (_bx * _cy - _by * _cx)))
	       
	       (dis (!! (square coeff-b) - 4 * coeff-a * coeff-c))
	       
	       (t1 (!! ((- coeff-b) - (sqrt dis)) / (2 * coeff-a)))
	       (t2 (!! ((- coeff-b) + (sqrt dis)) / (2 * coeff-a))))
	  (cond ((= coeff-a 0)
		 (list (!! (- coeff-c) / coeff-b)))
		((> dis 0)
		 (list t1 t2))
		((= dis 0)
		 (list (!! (- coeff-b) / (2 * coeff-a))))
		((< dis 0)
		 nil)))))))

(defun split-at-inflection-points (bezier)
  (let ((points (solve-inflection-points bezier))
	res)
    (cond ((null points)
	   (push bezier res))
	  (t (dolist (i (split-bezier-times bezier points))
	       (push i res))))
    (nreverse res)))

(defun single-curvature-beziers (beziers)
  (loop for i in beziers
     if (typep i 'bezier)
     appending (split-at-inflection-points i)
     else collecting i))
       
(defun print-inflection-points (points)
  (loop for i in points
       do (format t "~A: ~A~%" (first i) (second i))))

(defun inflection-tests ()
  (let ((b1 (make-bezier :a (2dp 0 0) :b (2dp 10 0)
			 :u (2dp 0 10) :v (2dp 10 10)))
	(b2 (make-bezier :a (2dp 0 0) :u (2dp 0 10)
			 :b (2dp 10 10) :v (2dp 10 0)))
	(b3 (make-bezier :a (2dp 0 0) :b (2dp 10 0)
			 :u (2dp 7 5) :v (2dp 3 5))))
    (format t "bezier: ~A: ~A~%" b1 (solve-inflection-points b1))

    (format t "bezier: ~A: ~A~%" b2 (solve-inflection-points b2))

    (format t "bezier: ~A: ~A~%" b3 (solve-inflection-points b3))

    (let ((beziers (split-at-inflection-points b3)))
      (dolist (b beziers)
	(format t "b: ~A points: ~A~%" b (solve-inflection-points b))))
    ))

(defun split-bezier-biarc-angle (bezier)
  (if (bezier-parallel-p bezier)
      (list bezier)
      (let ((angle  (bezier-biarc-angle bezier))
	    res)
	(cond ((<= angle 90)
	       (push bezier res))
	      (t (let ((beziers (split-bezier bezier 0.5)))
		   (dolist (b (mapcan #'split-bezier-biarc-angle beziers))
		     (push b res)))))
	(nreverse res))))
  

(defun split-bezier-for-arc (bezier)
  (let ((beziers (split-at-inflection-points bezier))
	(res))
    (mapcan #'split-bezier-biarc-angle beziers)))

(defun paper-examples ()
  (let ((b1 (make-bezier :a (2dp 16.9753 0.7421)
			 :u (2dp 18.2203 2.2238)
			 :v (2dp 21.0939 2.4017)
			 :b (2dp 23.1643 1.6148)))
	(b2 (make-bezier :a (2dp 17.5415 0.9003)
			 :u (2dp 18.4778 3.8448)
			 :v (2dp 22.4037 -0.9109)
			 :b (2dp 22.563 0.7782))))
    (format t "~A angle: ~A~%" b1 (bezier-biarc-angle b1))
    (format t "changes; ~A~%" (solve-inflection-points b1))
    (format t "~%")
    
    (format t "~A angle: ~A~%" b2 (bezier-biarc-angle b2))
    (format t "changes; ~A~%" (solve-inflection-points b2))
    (format t "split: ~A~%" (split-bezier-for-arc b2))

    ))

(defun bezier-parallel-p (bezier)
  (with-slots (a u v b) bezier
    (eq (line-intersection a u b v) :parallel)))

(defun bezier-to-arc (bezier)
  (with-slots (a u v b) bezier
    (let* ((line (make-line :a a :b b))
	   (l (line-length line)))
    (if (or (< l 0.1)
	    (bezier-parallel-p bezier))
	(list line)
	(mapcar #'bezier-arc (mapcan #'(lambda (x)
					 (split-bezier-times x '(0.25 0.5 0.75)))
				     (split-bezier-for-arc bezier)))))))

(defun curve-start (curve)
  (cond ((null curve)
	 (error "empty curve"))
	((listp curve)
	 (curve-start (first curve)))
	((typep curve 'line)
	 (line-a curve))
	((typep curve 'arc)
	 (arc-a curve))
	((typep curve 'bezier)
	 (bezier-a curve))
	(t (error "unknown curve elt: ~A" curve))))

(defun curve-to-arcs (curve)
  (mapcan #'(lambda (x) (cond ((typep x 'bezier)
			       (bezier-to-arc x))
			      (t (list x)))) curve))

