(in-package :gcode)

(defvar *cube-steps* 1)
(defvar *drill-steps* nil)
(defparameter *tool-diameter* 2)

(defun test-rel-cube ()
  (with-program ("fpp")
    (with-named-pass ("mill")
      (with-transform ((translation-matrix 10 -2))
				(goto-abs :x 0 :y 0)
				(with-tool-down ()
					(mill-u)
					(mill-r)
					(mill-d)
					(mill-l)
					#+nil(cube-top))))))

(defun cube-top ()
  (tool-up)
  (mill-r)
  
  (with-tool-down (*step-width*)
    ;; 3 nasen
    (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (repeat (*cube-steps*)
						(mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d))
    (mill-r++) (s-mill-round-u)
    
    (mill-r)
    (mill-u)
    
    (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
    (repeat (*cube-steps*)
						(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u--))
    (s-mill-round-r) (mill-u++)
    
    (s-mill-round-l) (mill-l)
    (s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
    (repeat (*cube-steps*)
						(s-mill-round-u) (mill-l++) (s-mill-round-d) (mill-l--))
    (s-mill-round-u) (mill-l++)
    
    (s-mill-round-d) (mill-d)
    (s-mill-round-l) (mill-bridge-d++) (s-mill-round-r) (mill-d--)
    (repeat (*cube-steps*)
						(s-mill-round-l) (mill-d++) (s-mill-round-r) (mill-d--))
    (s-mill-round-l) (mill-d++) (s-mill-round-r) (s-mill-round-d)))

(defun cube-bottom ()
  (with-tool-down (*step-width*)
    (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (repeat (*cube-steps*)
						(mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d))
    (mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (mill-r++) (s-mill-round-u) (mill-u)
    
    (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
    (repeat (*cube-steps*)
						(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u--))
    (s-mill-round-r) (mill-u++) (mill-l) (mill-l)
    
    (s-mill-round-u)
    (mill-bridge-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u)
    (repeat (*cube-steps*)
						(mill-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u))
    (mill-l++) (mill-l)
    
    (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
    (repeat (*cube-steps*)
						(mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l))
    (mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
    (mill-d++) (mill-d)))

(defun cube-1 ()
  (tool-up)
  (mill-r) (mill-u)
  
  (with-tool-down (*step-width*)
    (mill-r)
    (s-mill-round-d) (mill-bridge-r++) (s-mill-round-u) (mill-r--)
    (repeat (*cube-steps*)
						(s-mill-round-d) (mill-r++) (s-mill-round-u) (mill-r--))
    (s-mill-round-d) (mill-r++)
    
    (s-mill-round-u) (mill-u)
    (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
    (repeat (*cube-steps*)
						(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u--))
    (s-mill-round-r) (mill-u++) (s-mill-round-l)
    
    (s-mill-round-u) (mill-l++) (s-mill-round-d) (mill-l--)
    (s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
    (repeat (*cube-steps*)
						(s-mill-round-u) (mill-l++) (s-mill-round-d) (mill-l--))
    (s-mill-round-u) (mill-l++) (mill-d) 
    
    (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
    (repeat (*cube-steps*)
						(mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l))
    (mill-d++) (s-mill-round-r) (mill-d)))

(defun cube-2 ()
  (with-tool-down (*step-width*)
    (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (repeat (*cube-steps*)
						(mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d))
    (mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (mill-r++) (s-mill-round-u) (mill-u)
    
    (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
    (repeat (*cube-steps*)
						(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u--))
    (s-mill-round-r) (mill-u++) (mill-u)
    
    (mill-bridge-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u)
    (repeat (*cube-steps*)
						(mill-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u))
    (mill-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u)
    (mill-l++) (mill-l)
    
    (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
    (repeat (*cube-steps*)
						(mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l))
    (mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
    (mill-d++) (mill-d)))

(defun cube-3 ()
  (with-tool-down (*step-width*)
    (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (repeat (*cube-steps*)
						(mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d))
    (mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
    (mill-r++) (s-mill-round-u)
    
    (mill-u)
    (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
    (repeat (*cube-steps*)
						(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u--))
    (s-mill-round-r) (mill-u++) (mill-u)
    
    (mill-l++)
    (s-mill-round-d) (mill-l--)
    (s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
    (repeat (*cube-steps*)
						(s-mill-round-u) (mill-l++) (s-mill-round-d) (mill-l--))
    (s-mill-round-u) (mill-l++) (s-mill-round-d)
    
    (mill-d) (s-mill-round-l)
    (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
    (repeat (*cube-steps*)
						(mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l))
    (mill-d++) (mill-d)))    

(defun cube-4 ()
  (tool-up)
  (mill-r)(mill-u)
  (with-tool-down (*step-width*)
    
		(mill-r)
		(s-mill-round-d) (mill-bridge-r++) (s-mill-round-u) (mill-r--)
		(repeat (*cube-steps*)
						(s-mill-round-d) (mill-r++) (s-mill-round-u) (mill-r--))
		(s-mill-round-d) (mill-r++) (s-mill-round-u)

		(s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
		(repeat (*cube-steps*)
						(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u--))
		(s-mill-round-r) (mill-u++) (s-mill-round-l) (mill-u)
		(mill-l)

		(s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
		(repeat (*cube-steps*)
						(s-mill-round-u) (mill-l++) (s-mill-round-d) (mill-l--))
		(s-mill-round-u) (mill-l++) (s-mill-round-d)

		(mill-d)

		(s-mill-round-l) (mill-bridge-d++) (s-mill-round-r) (mill-d--)
		(repeat (*cube-steps*)
						(s-mill-round-l) (mill-d++) (s-mill-round-r) (mill-d--))
		(s-mill-round-l) (mill-d++) (s-mill-round-r)))

(defmacro with-inner-transformation (() &rest body)
  `(with-save-xy ()
     (with-transform ((translation-matrix
											 (+ (current-x) (/ *tool-diameter* 2) (* *step-width* 2))
											 (+ (current-y) (/ *tool-diameter* 2) (* *step-width* 2))))
       ,@body)))

(defun print-inner-dimensions (name)
  (let ((inner-x (+ (orig-current-x) (/ *tool-diameter* 2) (* *step-width* 2)))
				(inner-y (+ (orig-current-y) (/ *tool-diameter* 2) (* *step-width* 2)))
				(inner-width (* (1+ *cube-steps*) 2 *step-width*)))
    
    (format t "rectangle for ~A: x: ~A, y: ~A, width: ~A~%" name
						inner-x inner-y inner-width)))

(defun make-cube (&key (x 5) (y 5))
  (let* ((*step-width* 3.8)
				 (*tool-diameter* 2)
				 (*cube-steps* 12)
				 (*round-steps* nil)
				 (*cut-steps* nil)
				 (width (+ (* 2 (+ 3 *cube-steps*) *step-width*) 5))
				 (panels nil))

    (with-tool (*cube-tool*)
      (spindle-on)

      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)

      (with-current-xy (x y)
				(with-save-xy ()
					(cube-top))
				(with-inner-transformation ()
					(load-file "/Users/manuel/dxf/output-1.lisp"))
				(print-inner-dimensions "TOP"))
      
      (with-current-xy (x (+ y width))
				(with-save-xy ()
					(cube-bottom))
				(with-inner-transformation ()
					(load-file "/Users/manuel/dxf/output-2.lisp"))
				(print-inner-dimensions "BOTTOM"))
      
      (with-current-xy ((+ x width) y)
				(with-save-xy ()
					(cube-1))
				(with-inner-transformation ()
					(load-file "/Users/manuel/dxf/output-3.lisp"))
				(print-inner-dimensions "SIDE 1"))
      
      (with-current-xy ((+ x width) (+ y width))
				(with-save-xy ()
					(cube-2))
				(with-inner-transformation ()
					(load-file "/Users/manuel/dxf/output-4.lisp"))
				(print-inner-dimensions "SIDE 2"))
      
      (with-current-xy ((+ x (* 2 width)) y)
				(with-save-xy ()
					(cube-3))
				(with-inner-transformation ()
					(load-file "/Users/manuel/dxf/output-5.lisp"))
				(print-inner-dimensions "SIDE 3"))
	
      (with-current-xy ((+ x (* 2 width))
												(+ y width))
				(with-save-xy ()
					(cube-4))
				(with-inner-transformation ()
					(load-file "/Users/manuel/dxf/output-6.lisp"))
				(print-inner-dimensions "SIDE 4"))
      
      #+nil(nreverse *round-steps*)
      (nreverse *cut-steps*))))

#+nil
(defun circle-interior (num-circles)
  (with-inner-transformation ()
    (loop for x from 0 to (1- num-circles)
       collect (loop for y from 0 to (1- num-circles)
									collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																		 (+ 4 (* y (* 2 *step-width*)))
																		 4)))))

(defun cube-width ()
	(* 2 (+ 3 *cube-steps*) *step-width*))

(defun cube-inner-width ()
	(* 2 (+ 1 *cube-steps*) *step-width*))

(defun haye-interior ()
	(with-inner-transformation ()
		(with-transform ((scaling-matrix (/ (cube-inner-width) 480)))
			(let ((*p5-depth* 4))
				(p5-rect :x 80 :y 400 :width 320 :height 20)

				(p5-rect :x 30 :y 20 :width 20 :height 320)
				(p5-rect :x 430 :y 20 :width 20 :height 320))
			
			(let ((*p5-depth* 2))
				(p5-rect :x 160 :y 300 :width 160 :height 20)
				
				(p5-rect :x 130 :y 20 :width 20 :height 220)
				(p5-rect :x 330 :y 20 :width 20 :height 220)
				
				(p5-rect :x 220 :y 160 :width 40 :height 40)
				))
			

			
			))

(defun flo-interior ()
		(with-inner-transformation ()
			(with-transform ((scaling-matrix (/ (cube-inner-width) 480)))
				(let ((*p5-depth* 4))
					(loop 
						 for i from 80 below 440 by 80
						 for j from 1
						 do (if (oddp j)
										(progn (p5-rect :x (- i 20) :y 40 :width 40 :height 400))
										
										(p5-line :x1 i :y1 40
														 :x2 i  :y2 440)))))))

(defun panel-interior (panel)
	(with-inner-transformation ()
		(let ((widest (+ 20 (max (panel-width panel)
														 (panel-height panel)))))
			(with-transform ((scaling-matrix (/ (cube-inner-width) widest)))
				(format t "widest: ~A, min-x: ~A, width: ~A~%"
								widest (panel-min-x panel)
								(panel-width panel))
				(format t "widest: ~A, min-y: ~A, height: ~A~%"
								widest (panel-min-y panel)
								(panel-height panel))
				(with-transform ((translation-matrix (+ (- (panel-min-x panel))
																								(/ (- widest (panel-width panel)) 2))
																						 (+ (- (panel-min-y panel))
																								(/ (- widest (panel-height panel)) 2))))
					(execute-panel-gcode panel))))))

(defun svg-interior (svg &key depth)
	(let ((panel (svg-panel svg :depth depth)))
		(panel-interior panel)))
	


(defparameter *spiral-counter* 0)

(defparameter *spiral-hash* (make-hash-table))

;; spiral 4, 12, 14, 17,  21,  22

(defparameter *spiral-list* '())

(defun spiral-interior ()
	(with-tool-down ()
		#+nil(let ((panel (calculate-panel-code `((spiral-pass 2)))))
					 (setf (gethash (incf *spiral-counter*) *spiral-hash*) panel)
					 (panel-interior panel))
		#+nil(panel-interior (gethash (random-elt *spiral-list*) *spiral-hash*))
		(panel-interior (pop *spiral-list-panels*))
		))

(defun interior ()
	#+nil(svg-interior "/Users/manuel/illusion-orig.svg")
	#+nil(spiral-interior)
	#+nil(haye-interior)

	#+nil(flo-interior)

	#-nil(circles-interior)
	
	#+nil(weird-line-interior)
	
  #+nil (let ((r (random 100)))
    (cond ((> r 70)
					 (star-interior))
					((> r 50)
					 (circle-interior))
					((> r 30)
					 (weird-line-interior))
					(t (line-interior)))))

(defun circle-interior ()
  (with-inner-transformation ()
    (with-transform ((scaling-matrix 0.1))
      (dotimes (i 30)
				(let ((w (p5-random 10 100)))
					(p5-ellipse :x (p5-random 400) :y (p5-random 400) :width w :height w))))))

(defun random2 (low end)
	(+ low (random (- end low))))

(defun random-circles ()
	(do ((circles (list))
			 (cnt 0 (1+ cnt)))
			((or (> cnt 8000)
					 (= (length circles) 35)) circles)
		(let* ((radius (random2 30 200))
					(x (random2 (+ 30 (/ radius 2)) (- 440 (/ radius 2))))
					(y (random2 (+ 30 (/ radius 2)) (- 440 (/ radius 2)))))
			(when (every #'(lambda (circle)
								 (let* ((x2 (car circle))
												(y2 (cadr circle))
												(r2 (caddr circle))
												(d (distance x y x2 y2)))
									 (> d (+ radius r2))))
									 circles)
				(push (list x y radius) circles)))))
	

(defun circles-interior ()
	(with-inner-transformation ()
		(with-transform ((scaling-matrix (/ (cube-inner-width) 480)))
			(dolist (circle (random-circles))
				(apply #'p5-circle circle)))))

(defun line-interior ()
  (with-inner-transformation ()
    (with-transform ((scaling-matrix 0.1))
      (dotimes (i 30)
				(let ((w (p5-random 10 100)))
					(p5-rect :x (p5-random 400) :y (p5-random 400) :width w :height w))))))

(defun weird-line-interior ()
  (with-inner-transformation ()
    (with-transform ((scaling-matrix (/ (cube-inner-width) 480)))
      (loop with inc = 20
				 for i from 40 below 480 by 80
				 do (incf inc (p5-random -10 10))
				 (when (<= inc 20)
					 (setf inc 20))
				 (p5-line :x1 i :y1 (+ 40 (* 10 (- inc 20)))
									:x2 i  :y2 (- 440 (* 10 (- inc 20))))))))

(defun star (pts radius ctrl-radius)
  (let* ((angle (/ 360 pts))
				 (ctrl-angle1 (/ angle 3))
				 (ctrl-angle2 (* ctrl-angle1 2))
				 (width 200)
				 (height 200))
    (loop for i from 0 below pts
			 collect
			 (make-bezier :a (2dp (!! width / 2 + (cos (deg-to-radians angle)) * radius)
														(!! height / 2 + (sin (deg-to-radians angle)) * radius))
										:u (2dp (!! width / 2 + (cos (deg-to-radians (angle + ctrl-angle1)))
																* ctrl-radius / (cos (deg-to-radians ctrl-angle1)))
														(!! height / 2 + (sin (deg-to-radians (angle + ctrl-angle1)))
																* ctrl-radius / (cos (deg-to-radians ctrl-angle1))))
										:v (2dp (!! width / 2 + (cos (deg-to-radians (angle + ctrl-angle2)))
																* ctrl-radius / (cos (deg-to-radians ctrl-angle1)))
														(!! height / 2 + (sin (deg-to-radians (angle + ctrl-angle2)))
																* ctrl-radius / (cos (deg-to-radians ctrl-angle1))))
										:b (2dp (!! width / 2 + (cos (deg-to-radians (angle + 360 / pts))) *
																radius)
														(!! height / 2 + (sin (deg-to-radians (angle + 360 / pts))) *
																radius)))
			 do (incf angle (/ 360 pts)))))

(defun star-interior ()
  (let* ((a1 (p5-random 0 90))
				 (sign (sign (p5-random -10 10)))
				 (sign2 (sign (p5-random -10 10)))
				 (a2 (+ a1 (* sign2 (p5-random 30 60))))
				 (curve (curve-to-arcs (star (p5-random 5 15) (* sign a1) (* sign a2))))
				 (bbox (bounding-box curve)))
    (format t "bbox: ~A~%" bbox)
    (with-inner-transformation ()
      (with-transform ((scaling-matrix (/ 35 (bbox-width bbox))))
				(with-transform ((translation-matrix (+ (* (bbox-width bbox)
																									 1/7)
																								(- (bbox-left bbox)))
																						 (+ (* (bbox-width bbox)
																									 1/7)
																								(- (bbox-bottom bbox)))))
					(mill-curve curve))))))

(defun s-cube-top ()
  (with-named-pass ("circles")
    #+nil(interior))
  (cube-top))

(defun s-cube-1 ()
  (with-named-pass ("circles")
    (interior))
  (cube-1))

(defun s-cube-2 ()
  (with-named-pass ("circles")
    (interior))
  (cube-2))

(defun s-cube-3 ()
  (with-named-pass ("circles")
    (interior))
  (cube-3))

(defun s-cube-4 ()
  (with-named-pass ("circles")
    (interior))
  (cube-4))

(defun s-cube-bottom ()
  (cube-bottom))

(defun small-cube-panels ()
  (loop for f in '(s-cube-top s-cube-bottom s-cube-1 s-cube-2 s-cube-3 s-cube-4)
     collect (calculate-panel f)))

(defun small-cube-2-lines (&key (steps 3))
  (let ((*step-width* 4)
				(*tool-diameter* 2)
				(*cube-steps* steps))
	(with-program ("small-cube-2-lines")
		(with-tool (*cube-tool*)
				(spindle-on)
				(goto-abs :x 0 :y 0)
				(goto-abs :z *fly-height*)

				(format t "outer program: ~A~%" *current-program*)

				(with-transform ((translation-matrix 10 10))
					(let* ((panels (small-cube-panels))
								 (orders (order-panels panels
																			 '((1 2 3 4 5 6)) 
																			 10)))
						(with-transform ((translation-matrix 0 290))
							(loop for order in orders
								 for x = (second order)
								 for y = (third order)
								 for panel = (first order)
								 do (with-named-pass ("drills")
											(panel-drills x y panel))
									 (schedule-panel panel x y)))
						(with-transform ((translation-matrix 420 0))
							(with-transform ((rotation-matrix -90))
							(loop for order in orders
								 for x = (second order)
								 for y = (third order)
								 for panel = (first order)
								 do (with-named-pass ("drills")
											(panel-drills x y panel))
									 (schedule-panel panel x y))))))))))
								
					

(defun small-cube-schedule (&key (steps 3) schedule)
  (let ((*step-width* 4)
				(*tool-diameter* 2)
				(*cube-steps* steps))
		(format t "cube width: ~A~%" (cube-width))
    (with-program ("small-cube")
      (with-tool (*cube-tool*)
				(spindle-on)
				(goto-abs :x 0 :y 0)
				(goto-abs :z *fly-height*)

				(format t "outer program: ~A~%" *current-program*)
	
				(with-transform ((translation-matrix 10 10))
					(let* ((panels (small-cube-panels))
								 (orders (order-panels panels
																			 (if schedule
																					 schedule
																					 (elt (list '((1 2 3 4)
																												(1 2 3 4)
																												(5 6 1 2)
																												(3 4 5 6))
																											
																											'((1 2 3 4 5 6))

																											'((1 2 3 4)
																												(1 2 3 4)
																												(4 5 1 2)
																												(3 4 5 6))
																											
																											'((5 6)
																												(1 2 3 4))
																											
																											'((1 2 3 4 5 6)
																												(1 2 3 4 5 6)
																												(1 2 3 4 5 6))
																											
																											'(
																												(1 2 3 4 5 6)
																												(1 2 3 4 5 6)
																												))
																								2))
																			 10)))
						(format t "panels: ~A~%, orders: ~A~%" panels orders)
						(loop for order in orders
							 for x = (second order)
							 for y = (third order)
							 for panel = (first order)
							 do (with-named-pass ("drills")
										(panel-drills x y panel))
							 (schedule-panel panel x y))))

				#+nil(optimize-pass "circles")))))


(defun panel-drills (x y panel)
  (when (= x 0)
    (goto-abs :x (- x 6.5)
							:y (+ y (panel-height panel) 3.5))
    (circle-inline 1.4 :depth 3.8)
    (mill-abs :z *fly-height*))
  (when (= y 0)
    (goto-abs :x (+ x (panel-width panel) 3.5)
							:y (- y 6.5))
    (circle-inline 1.4 :depth 3.8)
		(mill-abs :z *fly-height*))
  (goto-abs :x (+ x (panel-width panel) 3.5)
						:y (+ y (panel-height panel) 3.5))
  (circle-inline 1.4 :depth 3.8)
  (mill-abs :z *fly-height*))

  
(defun small-cube (&key (x 0) (y 0))
  (let* ((*step-width* 8)
				 (*tool-diameter* 2)
				 (*cube-steps* 5)
				 (*round-steps* nil)
				 (num-circles 6)
				 (*cut-steps* nil)
				 (width (+ (* 2 (+ 3 *cube-steps*) *step-width*) 10)))

    (with-tool (*cube-tool*)
      (spindle-on)

      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)

      (with-current-xy (x y)
				(with-inner-transformation ()
					(loop for x from 0 to (1- num-circles)
						 collect (loop for y from 0 to (1- num-circles)
												collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																					 (+ 4 (* y (* 2 *step-width*)))
																					 4))))
				(with-save-xy ()
					(cube-top))
	
				(print-inner-dimensions "TOP"))
      
      (with-current-xy (x (+ y width))
				(with-inner-transformation ()
					(loop for x from 0 to (1- num-circles)
						 collect (loop for y from 0 to (1- num-circles)
												collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																					 (+ 4 (* y (* 2 *step-width*)))
																					 4))))
				(with-save-xy ()
					(cube-bottom))
				(print-inner-dimensions "BOTTOM"))
      
      (with-current-xy ((+ x width) y)
				(with-inner-transformation ()
					(loop for x from 0 to (1- num-circles)
						 collect (loop for y from 0 to (1- num-circles)
												collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																					 (+ 4 (* y (* 2 *step-width*)))
																					 4))))
				(with-save-xy ()
					(cube-1))


				(nreverse *round-steps*)
				(nreverse *cut-steps*)
     	
				(print-inner-dimensions "SIDE 1"))
      
      (with-current-xy ((+ x width) (+ y width))
				(with-inner-transformation ()
					(loop for x from 0 to (1- num-circles)
						 collect (loop for y from 0 to (1- num-circles)
												collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																					 (+ 4 (* y (* 2 *step-width*)))
																					 4))))
				(with-save-xy ()
					(cube-2))
				(print-inner-dimensions "SIDE 2"))
      
      (with-current-xy ((+ x (* 2 width)) y)
				(with-inner-transformation ()
					(loop for x from 0 to (1- num-circles)
						 collect (loop for y from 0 to (1- num-circles)
												collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																					 (+ 4 (* y (* 2 *step-width*)))
																					 4))))
				(with-save-xy ()
					(cube-3))
				(print-inner-dimensions "SIDE 3"))
	
      (with-current-xy ((+ x (* 2 width))
												(+ y width))
				(with-inner-transformation ()
					(loop for x from 0 to (1- num-circles)
						 collect (loop for y from 0 to (1- num-circles)
												collect (p5-circle (+ 4 (* x (* 2 *step-width*)))
																					 (+ 4 (* y (* 2 *step-width*)))
																					 4))))
				(with-save-xy ()
					(cube-4))
				(print-inner-dimensions "SIDE 4"))
      
      (nreverse *round-steps*)
      (nreverse *cut-steps*)
      )))