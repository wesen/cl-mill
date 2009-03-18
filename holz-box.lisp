(in-package :gcode)

(defparameter *kiefer-width* 4) ;; 4 mm kiefer holz

(defparameter *kiefer-tool*
  (make-instance 'tool
		 :diameter 2
		 :number 11
		 :feed-xy 600
		 :feed-z 240
		 :depth 1.5))

(defparameter *holz-box-wx* 32)
(defparameter *holz-box-wy* 60)
(defparameter *holz-box-height* 22)
(defparameter *holz-box-schiebedach* t)

(defun length-steps (length width)
  (floor (/ (1- length) width)))

(defun calc-box-width (len &optional (step-width *step-width*))
  (- len (* 6 step-width) (/ *tool-diameter* 1)))

(defun calc-box-steps (len &optional (step-width *step-width*))
  (floor (/ (/ (calc-box-width len step-width) step-width) 4)))

(defun holz-box-top (&key (wx *holz-box-wx*) (wy *holz-box-wy*) (height *holz-box-height*))
  (let* ((x-width (calc-box-width wx))
	 (y-width (calc-box-width wy))
	 (h-width (calc-box-width height))
	 (x-steps (calc-box-steps wx))
	 (y-steps (calc-box-steps wy))
	 (h-steps (calc-box-steps height))
	 (x-step-width (/ (/ x-width x-steps) 2))
	 (y-step-width (/ (/ y-width y-steps) 2))
	 (h-step-width (/ (/ h-width h-steps) 2)))

    (format t "wy: ~A, real width: ~A stepwidth: ~A~%"
	    wy (+ (* y-step-width y-steps)
		  (* 6 *step-width*))
	    y-step-width)
    (format t "wx: ~A, real width: ~A stepwidth: ~A~%"
	    wx (+ (* x-step-width x-steps)
		  (* 6 *step-width*))
	    x-step-width)
    
    (with-tool-down (*kiefer-width*)
      (if *holz-box-schiebedach*
	  (progn
	    (mill-r)(mill-r)(mill-r)
	    (repeat (x-steps)
		    (mill-r x-step-width)
		    (mill-r x-step-width))
	    (mill-r++)(mill-r)(mill-r)
	    (mill-u++)(s-mill-round-l)(mill-u--)
	    )
	  
	  (progn
	    
	    (tool-up)
	    (mill-r)
	    (tool-down :depth *kiefer-width*)
	    
	    ;; 3 nasen
	    (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
	    (repeat (x-steps)
		    (mill-r++ x-step-width) (s-mill-round-u)
		    (mill-r-- x-step-width) (s-mill-round-d))
	    (mill-r++) (s-mill-round-u) 
	    (mill-r)(mill-u)))
    
    
      (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
      (repeat (h-steps)
	      (s-mill-round-r) (mill-u++ h-step-width)
	      (s-mill-round-l) (mill-u-- h-step-width))
      (s-mill-round-r) (mill-u++)

      (s-mill-round-l) (mill-l)
      (repeat (x-steps)
	      (s-mill-round-u) (mill-l++ x-step-width)
	      (s-mill-round-d) (mill-l-- x-step-width))
      (s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
      (s-mill-round-u) (mill-l++)

      (s-mill-round-d) (mill-d)
      (s-mill-round-l)
      (mill-bridge-d++) (s-mill-round-r) (mill-d--)
      (repeat (h-steps)
	      (s-mill-round-l) (mill-d++ h-step-width)
	      (s-mill-round-r) (mill-d-- h-step-width))
      (s-mill-round-l) (mill-d++)
    
      (if *holz-box-schiebedach*
	  (progn
	    (mill-d)
	    )
	  (progn
	    (s-mill-round-r) (s-mill-round-d))))))
  
(defun holz-box-bottom (&key (wx *holz-box-wx*) (wy *holz-box-wy*) (height *holz-box-height*))
  (let* ((x-width (calc-box-width wx))
	 (y-width (calc-box-width wy))
	 (h-width (calc-box-width height))
	 (x-steps (calc-box-steps wx))
	 (y-steps (calc-box-steps wy))
	 (h-steps (calc-box-steps height))
	 (x-step-width (/ (/ x-width x-steps) 2))
	 (y-step-width (/ (/ y-width y-steps) 2))
	 (h-step-width (/ (/ h-width h-steps) 2)))
    
    (with-tool-down (*kiefer-width*)
      (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
      (repeat (y-steps)
	      (mill-r++ y-step-width) (s-mill-round-u)
	      (mill-r-- y-step-width) (s-mill-round-d))
      (mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
      (mill-r++) (s-mill-round-u) (mill-u)
    
      (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
      (repeat (x-steps)
	      (s-mill-round-r) (mill-u++ x-step-width)
	      (s-mill-round-l) (mill-u-- x-step-width))
      (s-mill-round-r) (mill-u++) (mill-l) (mill-l)
    
      (s-mill-round-u)
      (mill-bridge-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u)
      (repeat (y-steps)
	      (mill-l++ y-step-width) (s-mill-round-d)
	      (mill-l-- y-step-width) (s-mill-round-u))
      (mill-l++) (mill-l)
    
      (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
      (repeat (x-steps)
	      (mill-d++ x-step-width) (s-mill-round-r)
	      (mill-d-- x-step-width) (s-mill-round-l))
      (mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
      (mill-d++) (mill-d))))

(defun holz-box-1 (&key (wx *holz-box-wx*) (wy *holz-box-wy*) (height *holz-box-height*))
  (let* ((x-width (calc-box-width wx))
	 (y-width (calc-box-width wy))
	 (h-width (calc-box-width height))
	 (x-steps (calc-box-steps wx))
	 (y-steps (calc-box-steps wy))
	 (h-steps (calc-box-steps height))
	 (x-step-width (/ (/ x-width x-steps) 2))
	 (y-step-width (/ (/ y-width y-steps) 2))
	 (h-step-width (/ (/ h-width h-steps) 2)))
    
    (if *holz-box-schiebedach*
	(progn
	  (with-tool-down (*kiefer-width*)
	    )
	  )

	(progn
	  (tool-up)
	  (mill-r) (mill-u)
	
	  (with-tool-down (*kiefer-width*)
	    (mill-r)
	    (s-mill-round-d) (mill-bridge-r++) (s-mill-round-u) (mill-r--)
	    (repeat (x-steps)
		    (s-mill-round-d) (mill-r++ x-step-width)
		    (s-mill-round-u) (mill-r-- x-step-width))
	    (s-mill-round-d) (mill-r++)
	  
	    (s-mill-round-u) (mill-u)
	    (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
	    (repeat (y-steps)
		    (s-mill-round-r) (mill-u++ y-step-width)
		    (s-mill-round-l) (mill-u-- y-step-width))
	    (s-mill-round-r) (mill-u++) (s-mill-round-l)
	  
	    (s-mill-round-u) (mill-l++) (s-mill-round-d) (mill-l--)
	    (s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
	    (repeat (x-steps)
		    (s-mill-round-u) (mill-l++ x-step-width)
		    (s-mill-round-d) (mill-l-- x-step-width))
	    (s-mill-round-u) (mill-l++) (mill-d) 
	  
	    (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
	    (repeat (y-steps)
		    (mill-d++ y-step-width) (s-mill-round-r)
		    (mill-d-- y-step-width) (s-mill-round-l))
	    (mill-d++) (s-mill-round-r) (mill-d))))))
      
(defun holz-box-2 (&key (wx *holz-box-wx*) (wy *holz-box-wy*) (height *holz-box-height*))
  (let* ((x-width (calc-box-width wx))
	 (y-width (calc-box-width wy))
	 (h-width (calc-box-width height))
	 (x-steps (calc-box-steps wx))
	 (y-steps (calc-box-steps wy))
	 (h-steps (calc-box-steps height))
	 (x-step-width (/ (/ x-width x-steps) 2))
	 (y-step-width (/ (/ y-width y-steps) 2))
	 (h-step-width (/ (/ h-width h-steps) 2)))
    
    (with-tool-down (*kiefer-width*)
      (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
      (repeat (x-steps)
	      (mill-r++ x-step-width) (s-mill-round-u)
	      (mill-r-- x-step-width) (s-mill-round-d))
      (mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
      (mill-r++) (s-mill-round-u) (mill-u)
    
      (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
      (repeat (h-steps)
	      (s-mill-round-r) (mill-u++ h-step-width)
	      (s-mill-round-l) (mill-u-- h-step-width))
      (s-mill-round-r) (mill-u++) (mill-u)

      (if *holz-box-schiebedach*
	  (progn
	    (mill-l)(mill-l)
	    (repeat (x-steps)
		    (mill-l x-step-width)
		    (mill-l x-step-width))
	    (mill-l)(mill-l)(mill-l)(mill-l++)
	    )
	  (progn
	    (mill-bridge-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u)
	    (repeat (x-steps)
		    (mill-l++ x-step-width) (s-mill-round-d)
		    (mill-l-- x-step-width) (s-mill-round-u))
	    (mill-l++) (s-mill-round-d) (mill-l--) (s-mill-round-u)
	    (mill-l++) (mill-l)))
    
      (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
      (repeat (h-steps)
	      (mill-d++ h-step-width) (s-mill-round-r)
	      (mill-d-- h-step-width) (s-mill-round-l))
      (mill-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
      (mill-d++) (mill-d))))

(defun holz-box-3 (&key (wx *holz-box-wx*) (wy *holz-box-wy*) (height *holz-box-height*))
  (let* ((x-width (calc-box-width wx))
	 (y-width (calc-box-width wy))
	 (h-width (calc-box-width height))
	 (x-steps (calc-box-steps wx))
	 (y-steps (calc-box-steps wy))
	 (h-steps (calc-box-steps height))
	 (x-step-width (/ (/ x-width x-steps) 2))
	 (y-step-width (/ (/ y-width y-steps) 2))
	 (h-step-width (/ (/ h-width h-steps) 2)))
    
    (with-tool-down (*kiefer-width*)
      (if *holz-box-schiebedach*
	  (progn
	    (tool-up)
	    (mill-u)
	    (mill-r)
	    (tool-down :depth *kiefer-width*)
	    (mill-r)
	    (repeat (y-steps)
		    (mill-r y-step-width)
		    (mill-r y-step-width))
	    (mill-r)(mill-r)(mill-r++)
	    ;; (mill-u)
	    )

	  (progn
	    (mill-bridge-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
	    (repeat (y-steps)
		    (mill-r++ y-step-width) (s-mill-round-u)
		    (mill-r-- y-step-width) (s-mill-round-d))
	    (mill-r++) (s-mill-round-u) (mill-r--) (s-mill-round-d)
	    (mill-r++) (s-mill-round-u)))
    
      (mill-u)
      (repeat (h-steps)
	      (s-mill-round-r) (mill-u++ h-step-width)
	      (s-mill-round-l) (mill-u-- h-step-width))
      (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
      (s-mill-round-r) (mill-u++) (mill-u)
    
      (mill-l++)
      (s-mill-round-d) (mill-l--)
      (s-mill-round-u) (mill-bridge-l++) (s-mill-round-d) (mill-l--)
      (repeat (y-steps)
	      (s-mill-round-u) (mill-l++ y-step-width)
	      (s-mill-round-d) (mill-l-- y-step-width))
      (s-mill-round-u) (mill-l++) (s-mill-round-d)
    
      (mill-d) (s-mill-round-l)
      (repeat (h-steps)
	      (mill-d++ h-step-width) (s-mill-round-r)
	      (mill-d-- h-step-width) (s-mill-round-l))
      (mill-bridge-d++) (s-mill-round-r) (mill-d--) (s-mill-round-l)
      (mill-d++)

      (if (not *holz-box-schiebedach*)
	  (progn
	    ;;	  (mill-d)
	    )
	  (progn
	    (s-mill-round-r) (mill-d))))))


(defun holz-box-4 (&key (wx *holz-box-wx*) (wy *holz-box-wy*) (height *holz-box-height*))
  (let* ((x-width (calc-box-width wx))
	 (y-width (calc-box-width wy))
	 (h-width (calc-box-width height))
	 (x-steps (calc-box-steps wx))
	 (y-steps (calc-box-steps wy))
	 (h-steps (calc-box-steps height))
	 (x-step-width (/ (/ x-width x-steps) 2))
	 (y-step-width (/ (/ y-width y-steps) 2))
	 (h-step-width (/ (/ h-width h-steps) 2)))
    
    (with-tool-down (*kiefer-width*)

      (tool-up)
      (mill-r)(mill-u)
      (tool-down :depth *kiefer-width*)

      (if *holz-box-schiebedach*
	  (progn
	    ;;	  (mill-d)
	    (mill-r)(mill-r)
	    (repeat (y-steps)
		    (mill-r y-step-width)
		    (mill-r y-step-width))
	    (mill-r)(mill-r++)(mill-u)
	    ;;	  (mill-u)
	    )
	  (progn
	    (s-mill-round-d) (mill-bridge-r++) (s-mill-round-u) (mill-r--)
	    (repeat (y-steps)
		    (s-mill-round-d) (mill-r++ y-step-width)
		    (s-mill-round-u) (mill-r-- y-step-width))
	    (s-mill-round-d) (mill-r++) (s-mill-round-u) (mill-r)
	    (mill-u)))
    
      (repeat (h-steps)
	      (s-mill-round-r) (mill-u++ h-step-width)
	      (s-mill-round-l) (mill-u-- h-step-width))
      (s-mill-round-r) (mill-bridge-u++) (s-mill-round-l) (mill-u--)
      (s-mill-round-r) (mill-u++) (s-mill-round-l)
    
      (mill-l)
    
      (repeat (y-steps)
	      (s-mill-round-u) (mill-l++ y-step-width)
	      (s-mill-round-d) (mill-l-- y-step-width))
      (s-mill-round-u) (mill-bridge-l++) 
      
      (s-mill-round-d) (mill-l--)
      (s-mill-round-u) (mill-l++) (s-mill-round-d)


      (mill-d)
      (s-mill-round-l) (mill-bridge-d++) (s-mill-round-r) (mill-d--)
      (repeat (h-steps)
	      (s-mill-round-l) (mill-d++ h-step-width)
	      (s-mill-round-r) (mill-d-- h-step-width))
      (s-mill-round-l) (mill-d++) (s-mill-round-r)
    
      )))

(defun holz-box-program (&key (wx 30) (wy *holz-box-wy*) (height 20))

  (with-program ("holz-box")
    (with-tool (*kiefer-tool*)
      (tool-up)
      (goto-abs :x 0 :y 0)
      (with-named-pass ("1")
	(holz-box-4)))))



(defun holz-box-panels (&key (wx 30) (wy *holz-box-wy*) (height 20))
  (loop for f in '(holz-box-top
		   holz-box-bottom
		   ;; holz-box-1
		   holz-box-2
		   holz-box-3
		   holz-box-4)
     collect (calculate-panel f)))

(defun holz-box-schedule (&key (wx 30) (wy *holz-box-wy*) (height 20))
  (let ((*kiefer-width* 3.8)
	(*tool-diameter* 2)
	(*cube-steps* 5)
	(*holz-box-wx* wx)
	(*holz-box-wy* wy)
	(*holz-box-height* height))
    
  (with-program ("small-cube")
    (with-tool (*cube-tool*)
      (spindle-on)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
	
      (with-transform ((translation-matrix 0 20))
	(let* ((panels (holz-box-panels :wx wx :wy wy :height height))
	       (orders (order-panels panels '((5))
				     #+nil'((2 )
					    (3 4)
					    (1 5))

				     10)))
	  (loop for order in orders
	     for x = (second order)
	     for y = (third order)
	     for panel = (first order)
	     do (with-named-pass ("drills")
		  (panel-drills x y panel))
	     (schedule-panel panel x y))))))))


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



