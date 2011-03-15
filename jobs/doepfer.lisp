(in-package :gcode)

(defparameter *doepfer-depth* 1.5
(defparameter *doepfer-rail-depth* 3)

(defun doepfer-frontplate-frame (n &key drill)
	(unless drill
		(setf drill (min 1 (1- n))))

	(format t "drill: ~A~%" drill)
	(with-named-pass ("doepfer-outline")
		(with-tool (*alu-tool*)
			(rectangle-outline-xy :x 0 :y 0
														:width (- (* n 5.08) 0.08)
														:height 128.5
														:depth *doepfer-depth*)))
	
	(with-named-pass ("doepfer-drills")
		(let ((i drill))
			(with-tool (*alu-tool*)
				(drill :x (+ (* i 5.08)
										 (/ 5.08 2))
							 :y 3
							 :diameter 3.2
							 :depth *doepfer-depth*)
				(drill :x (+ (* i 5.08)
										 (/ 5.08 2))
							 :y 125.5
							 :diameter 3.2
							 :depth *doepfer-depth*))))

	(with-named-pass ("pcb-drills")
		(with-tool (*alu-tool*)
			))
	)

(defun doepfer-frontplate-rail (n)
	(let ((rail-height 5))
		(with-named-pass ("doepfer-outline")
			(with-tool (*alu-tool*)
				(rectangle-outline-xy :x 0 :y 0
															:width (+ 7.5 (- (* n 5.08) 0.08) 7.5)
															:height rail-height
															:depth *doepfer-rail-depth*)))
	
	(with-named-pass ("doepfer-drills")
		(dotimes (i n)
			(with-tool (*alu-tool*)
				(drill :x (+ 7.5 (* i 5.08))
							 :y (/ rail-height 2)
							 :diameter 3.2
							 :depth *doepfer-rail-depth*))))

	(with-named-pass ("pcb-drills")
		(with-tool (*alu-tool*)
			))
	))

(defun doepfer-test-rail (n)
	(with-program ("doepfer-test")
		(with-tool (*alu-tool*)
			(goto :x 0 :y 0 :z *fly-height*)
			(doepfer-frontplate-rail n))))

(defun doepfer-test-panel (n)
	(with-program ("doepfer-test")
		(with-tool (*alu-tool*)
			(with-transform ((translation-matrix 20 3))
				(goto :x 0 :y 0 :z *fly-height*)
				(doepfer-frontplate-frame n)))))