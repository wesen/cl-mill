(in-package :gcode)

(defvar *spiral-scaling* 1)
(defvar *spiral-flipping* 1)
(defvar *spiral-length* 0)

(defmacro with-spiral-scaling ((s) &rest body)
	`(with-transform ((scaling-matrix ,s))
		 (let ((*spiral-scaling* (* ,s *spiral-scaling*)))
			 ,@body)))

(defmacro with-spiral-rotation ((s) &rest body)
	`(with-transform ((rotation-matrix (* *spiral-flipping* ,s)))
		 ,@body))

(defmacro with-spiral-flip ((s) &rest body)
	`(let ((*spiral-flipping* (* -1 *spiral-flipping*)))
		 (with-spiral-rotation (,s)
			 ,@body)))

(defmacro with-spiral-rot-and-scale ((s r) &rest body)
	`(with-spiral-scaling (,s)
		 (with-spiral-rotation (,r)
			 ,@body)))

(defmacro probability-dispatch (&rest funcs)
	(let ((r (gensym))
				(cnt 0)
				(sum (loop for (prob . code) in funcs
									summing prob)))
		
		`(let ((,r (random ,sum)))
			 (cond
				 ,@(loop for (prob . code) in funcs
							do (incf cnt prob)
							collect `((<= ,r ,cnt)
												,@code))))))

(defun spiral-flow (&optional (step-size 1))
	(unless (or (> *spiral-length* (+ 100 (* 100 (* *spiral-scaling* *spiral-scaling*)) (random 60))))
		(if (< *spiral-scaling* 0.2)
				(progn
					(mill-rel :x step-size)
					(with-rel-back-xy ()
						(let ((*spiral-length* (1+ *spiral-length*)))
							(with-spiral-rotation (3)
								(with-spiral-scaling (0.994)
									(spiral-flow))))))
				
				(probability-dispatch
				 (0.005
					(let ((*spiral-length* 0))
						(with-rel-back-xy ()
							(with-spiral-flip (2)
								(spiral-flow)))))
				 
				 (0.0008
					(let ((*spiral-length* 0))
						(with-rel-back-xy ()
							(with-spiral-flip (-3)
								(spiral-flow)))))
				 
				 (0.02
					(if (and (> *spiral-scaling* 0.3)
									 (< *spiral-length* 100)
									 (> *spiral-length* 40))
							(progn
								(mill-rel :x step-size)
								(let ((*spiral-length* 0))
									(with-rel-back-xy ()
										(with-spiral-flip (15)
											(with-spiral-scaling ((+ 0.8 (random 0.2)))
												(spiral-flow)))))
								
								(with-rel-back-xy ()
									(let ((*spiral-length* 0))
										(with-spiral-rotation (-25)
											(with-spiral-scaling ((+ 0.4 (random 0.4)))
												(spiral-flow))))))
							
							(spiral-flow)))
				 
				 (1
					(mill-rel :x step-size)
					(with-rel-back-xy ()
						(let ((*spiral-length* (1+ *spiral-length*)))
							(with-spiral-rotation (3)
								(with-spiral-scaling (0.994)
									(spiral-flow))))))))))
	
(defun spiral-pass (&optional (step-size 1))
	(with-named-pass ("spiral")
		(let ((*spiral-scaling* 1)
					(*spiral-flipping* 1))
			(with-tool-down  (2)
				(with-rel-back-xy ()
					(spiral-flow step-size))
				(with-rel-back-xy ()
					(with-spiral-flip (95)
						#+nil(spiral-flow step-size)))
				(with-rel-back-xy ()
					(with-spiral-flip (180)
						(spiral-flow step-size)))))))			
		

(defun spiral-program ()
	(with-program ("spiral")
		(with-tool (*cube-tool*)
			(spindle-on)
			(goto-abs :x 0 :y 0)
			(goto-abs :z *fly-height*)

			(spiral-pass 2))))
