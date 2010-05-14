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

(defvar *spiral-program* nil)

(defmacro with-my-rel-back-xy (() &rest body)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,x (orig-current-x))
					 (,y (orig-current-y)))
			 ;;       (format t "x before: ~A, y before: ~A~%" ,x ,y)
       ,@body
;;       (format t "x after ~A, y after: ~A~%" (orig-current-x) (orig-current-y))
       (my-mill-rel :x (- ,x (orig-current-x))
										:y (- ,y (orig-current-y))))))

(defun my-mill-rel (&key (x 0) (y 0) (z 0) f)
	(let* ((vec (transform-vector (+ (orig-current-x) x)
																(+ (orig-current-y) y)
																*current-transform*))
				 (step
					`(mill-abs :x ,(first vec)
										 :y ,(second vec)
										 :z ,(+ (current-z) z)
										 :f ,f)))
;;		(format t "step: ~A ~A ~A~%" step (orig-current-y) y)
		(push step *spiral-program*))
	(mill-rel :x x :y y :z z :f f))
	
(defun spiral-flow (&optional (step-size 1))
	(unless (or (> *spiral-length*
								 (+ 80 (* 120 (* *spiral-scaling* *spiral-scaling* *spiral-scaling*)) (random 60))))
		(if (< *spiral-scaling* 0.2)
				(progn
					(my-mill-rel :x step-size)
					(with-my-rel-back-xy ()
						(let ((*spiral-length* (1+ *spiral-length*)))
							(with-spiral-rotation (3)
								(with-spiral-scaling (0.994)
									(spiral-flow))))))
				
				(probability-dispatch
				 (0.005
					(let ((*spiral-length* 0))
						(with-my-rel-back-xy ()
							(with-spiral-flip (2)
								(spiral-flow)))))
				 
				 #+nil(0.0008
					(let ((*spiral-length* 0))
						(with-my-rel-back-xy ()
							(with-spiral-flip (-3)
								(spiral-flow)))))

				 (0.2
					(if (and (> *spiral-scaling* 0.3)
									 (< *spiral-length* 100)
									 (> *spiral-length* 40))
							(progn
								(my-mill-rel :x step-size)
								(let ((*spiral-length* 0))
									(with-my-rel-back-xy ()
										(with-spiral-flip (6)
											(with-spiral-scaling ((+ 0.8 (random 0.2)))
												(spiral-flow)))))
								
								(with-my-rel-back-xy ()
									(let ((*spiral-length* 0))
										(with-spiral-rotation (-25)
											(with-spiral-scaling ((+ 0.4 (random 0.4)))
												(spiral-flow))))))
							
							(spiral-flow)))
				 
				 (1
					(my-mill-rel :x step-size)
					(with-my-rel-back-xy ()
						(let ((*spiral-length* (1+ *spiral-length*)))
							(with-spiral-rotation (3)
								(with-spiral-scaling (0.994)
									(spiral-flow))))))))))
	
(defun spiral-pass (&optional (step-size 1))
	(with-named-pass ("spiral")
		(let ((*spiral-scaling* 1)
					(*spiral-flipping* 1))
			(with-tool-down  ()
				(with-my-rel-back-xy ()
					(spiral-flow step-size))
				(with-my-rel-back-xy ()
					(with-spiral-flip (95)
						#+nil(spiral-flow step-size)))
				(with-my-rel-back-xy ()
					(with-spiral-flip (180)
						(spiral-flow step-size)))))))

(defun spiral-program ()
	(with-program ("spiral")
		(with-tool (*cube-tool*)
			(spindle-on)
			(goto-abs :x 0 :y 0)
			(goto-abs :z *fly-height*)

			(spiral-pass 2))))

(defun spiral-panel-program (panel)
	(with-program ("spiral")
		(with-tool (*cube-tool*)
			(with-named-pass ("spiral")
				(with-transform ((translation-matrix (- (panel-min-x panel))
																						 (- (panel-min-y panel))))
					(execute-panel-gcode panel))))))

(defparameter *spiral-list-panels* (list))

(defparameter *spiral-hash* (make-hash-table))

(defun max-hash-index (hash)
	(loop with max = 0
			 for key being the hash-keys of hash
			 do (when (> key max)
						(setf max key))
			 finally (return max)))

(defun random-elts (list &optional (n (length list)))
	(when (> n (length list))
		(error "wrong number of random elements for list"))
;;	(format t "n: ~A ~A~%" n (length list))
	(do ((nums (list)))
			((<= n 0) (mapcar #'(lambda (x) (elt list x)) nums))
		(let ((x (random (length list))))
;;			(format t "n: ~A, x: ~A nums: ~A~%" n x nums)
			(unless (member x nums)
				(push x nums)
				(decf n)))))

(defun create-spirals (n dir)
	(ensure-directories-exist dir)
	(loop
			 with *current-tool* = *cube-tool*
		 with start = (max-hash-index *spiral-hash*)
		 for i from (1+ start) below (+ 1 start n)
		 for panel = (calculate-panel-code '((spiral-pass 2)))
		 for program = (spiral-panel-program panel)
		 do (program-to-pdf program (make-pathname :name (format nil "spiral-~A" i) :type "pdf" :defaults dir))
			 (setf (gethash i *spiral-hash*) panel)))
	
			 
(defun test-spirals ()
	(loop for i from 1
			 for panel = (calculate-panel-code '((spiral-pass 2)))
			 do (program-to-pdf (spiral-panel-program panel) "/Users/manuel/spiral.pdf")
			 (asdf:run-shell-command "open /Users/manuel/spiral.pdf")
			 (asdf:run-shell-command "open -a aquamacs")
			 (let ((i (read)))
				 (if i
						 (push panel *spiral-list-panels*)))))

(defun good-spirals ()
	(setf *spiral-list-panels* (mapcar #'(lambda (x) (gethash x *spiral-hash*)) (random-elts *good-spirals*))))


(defparameter *good-spirals*
	'(1 9 15  18 23 33 35 38 67 76 80 83 85 99 108 118 121 122 126 131 133 134 164 171 172 174 180 181 195 199))
		6 7 