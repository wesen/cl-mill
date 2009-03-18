(in-package :gcode)

(defun minicommand-panels ()
  (let ((*frontplate-elements* nil))
    (let ((normal-panels
	   (loop for f in '("minicommand.lisp" "minicommand-ioboard.lisp")
	      for filename = (format nil "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/~A" f)
	      collect (calculate-panel-file filename))))

      (push `(with-tool (*pcb-tool*)
	       (goto-abs :x 19.5 :y 47)
	       (rectangle-inline 71 26 :depth 3.8)
	       (goto-abs :x -2 :y -2)
	       (rectangle-outline 114 92 :depth 3.8))
	    *frontplate-elements*)
      (setf *frontplate-elements* (nreverse *frontplate-elements*))

      (append normal-panels
	      (list (calculate-panel-code *frontplate-elements* :passname "frontplate"))))))


;; CI-11 7 mm radius
;; 3FTL06 10 mm radius

(defvar *frontplate-program*)
(defvar *frontplate-elements*)

(defparameter *frontplate-depth* 2.4)
(defvar *frontplate-top* t)
(defvar *frontplate-side* nil)

(defun frontplate-element (&key name package x y angle)
  (with-named-pass ("frontplate")
    (cond ((string= package "3FTL06")
	   (when *frontplate-top*
	     (drill :x x :y y :diameter 10.5 :depth *frontplate-depth*)))
	  ((string= package "CI-11")
	   (when *frontplate-top*
	   (drill :x x :y y :diameter 7.5 :depth *frontplate-depth*)))
	  ((string= package "LED5MM")
	   (when *frontplate-top*
	   (drill :x x :y y :diameter 5.5 :depth *frontplate-depth*)))
	  ((string= package "POWER")
	   (when *frontplate-top*
	     ;; orig
	     (progn
	       (goto-abs :x (- x 7.25) :y (- y 4.25))
	       (rectangle-inline 14 7.5 :depth *frontplate-depth*)
	       )))
	  
	  ((string= package "MAB5SH")
	   (when *frontplate-side*
	   (drill :x 11.5 :y (- x 1.0) :diameter 18.5 :depth *frontplate-depth*)))

	  ((string= package "DCJ0202")
	   (when *frontplate-side*
	     #+nil
	     (progn ;; orig
	       (goto-abs :x 0 :y (+ y 5))
	       (rectangle-inline 12 10 :depth *frontplate-depth*))
	     (progn
	       (goto-abs :x 4 :y (+ y 5.5))
	       (rectangle-inline 8.5 8 :depth *frontplate-depth*))))
	  
	  ((string= package "DISPLAY-TEXT-C1624A")
	   (when *frontplate-top*
	     (progn (goto-abs :x (- x 32.6) :y (- y 16.9))
		    (rectangle-inline 71.5 26.5 :depth *frontplate-depth*)))))))

(defun test-file (&key (x 0) (y 0))
  (let ((*frontplate-elements* nil))
    (with-program ("file-tool")
      (with-tool (*pcb-tool*)

	(let* ((panels (minicommand-panels))
	       (orders (order-panels panels '((1 2 3)) 10)))
	  
	  (with-transform ((translation-matrix x y))
	    (loop for order in orders
	       for x = (second order)
	       for y = (third order)
	       for panel = (first order)
	       do (with-named-pass ("drill-fix")
		    (with-tool (*pcb-tool*)
		      (panel-drills x y panel)))
		 (schedule-panel panel x y)))))
      (optimize-pass "drills"))))

(defun test-minicommand (&key (x 2) (y 2))
  (with-program ("file-tool")
    (with-tool ((make-instance 'tool :diameter 2 :depth 4 :number 6 :feed-xy 600 :feed-z 240))
      (with-transform ((translation-matrix 0 0))
	(with-transform ((translation-matrix 90 0))
	  (with-transform ((rotation-matrix -90))
	    (with-transform ((translation-matrix 5 0.5))
	      
	      (load-file "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand-ioboard.lisp"))))
	
	#+nil(with-named-pass ("frontplate")
	  (goto-abs :x 0 :y 0)
	  (rectangle-inline 95 120 :depth 4)))


	    )))

(defparameter *alu-tool*
  (make-instance 'tool :diameter 2 :depth 1.3 :number 8 :feed-xy 500 :feed-z 100))

(defun minicommand-casing-side-top-hammond-first ()
  (let ((tool *alu-tool*)
	(*frontplate-depth* 3.0))
    (with-program ("casing")
      (with-named-pass ("frontplate")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      
      
	(with-named-pass ("mill")
	  (with-tool (*alu-tool*)
	    (with-transform ((translation-matrix 2.1 8.5))
	      (let ((*eagle-drills-p* nil)
		    (*frontplate-top* nil)
		    (*frontplate-side* t))
		(load-file "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand.lisp"))))))))
    

(defun minicommand-casing-side-top ()
  (let ((tool *alu-tool*)
	(*frontplate-depth* 3.6)) ;; abschleif tiefe 2.0 mm oder so
	
    (with-program ("casing")
      (with-named-pass ("umrandung")
	(goto-abs :x 0 :y 0)
	(rectangle 30 119))

      (with-named-pass ("frontplate")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      
      

	(with-named-pass ("mill")
	  (with-tool (*alu-tool*)
	  (with-transform ((translation-matrix 6.5 4)) ;; 4 pcb zu rand + 1
	    (let ((*eagle-drills-p* nil)
		  (*frontplate-top* nil)
		  (*frontplate-side* t))
	      (load-file "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand.lisp"))))))))

(defun minicommand-frontplate (tool)
      (with-named-pass ("frontplate")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      
      (with-tool (tool)
	(with-transform ((translation-matrix 2.8 -0.75))
	  (with-transform ((translation-matrix 90 0))
	    (with-transform ((rotation-matrix -90))
	      (with-transform ((translation-matrix 5 3))
		(load-file "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand-ioboard.lisp"))))))
      
      #-nil
      (with-named-pass ("engrave")
	(let* ((curves (mapcar #'curve-to-arcs (interpret-svg (load-svg "/Users/manuel/test.svg"))))
	       (wbbox (bounding-box curves)))
	  (format t "wbbox: ~A width: ~A height; ~A~%" wbbox
		  (bbox-width wbbox)
		  (bbox-height wbbox))
	  
	  (with-tool (*engrave-tool*)
	    (with-transform ((translation-matrix 42.3 46.8))
	      (with-transform ((translation-matrix (+ (bbox-height wbbox)) 0))
		(with-transform ((rotation-matrix -90))
		  (with-transform ((translation-matrix (- (2d-point-x (line-a wbbox)))
						       (- (2d-point-y (line-a wbbox)))))
		    (dolist (curve curves)
		      (goto-abs :z *fly-height*)
		      (let ((start (curve-start curve)))
			(goto-abs :x (2d-point-x start)
				  :y (2d-point-y start)))
		      (with-tool-down ()
			(mill-curve curve)
			))))))))))

  

(defun test-minicommand-casing ()
  (let ((tool *alu-tool*))
    
    
    (with-program ("casing")
      (minicommand-frontplate *alu-tool*)
      (with-named-pass ("umrandung")
	(goto-abs :x 0 :y 0
		  )
	(rectangle-inline 93.5 119 :depth 4)))


	    ))

(defun test-minicommand-casing-2 ()
  (with-program ("casing2")
    (minicommand-frontplate *alu-tool*)
    (with-transform ((translation-matrix 0 122))
      (minicommand-frontplate *alu-tool*))))

(defun test-minicommand-casing-3 ()
  (with-program ("casing2")
    (minicommand-frontplate *alu-tool*)
    (with-transform ((translation-matrix 0 122))
      (minicommand-frontplate *alu-tool*))
    (with-transform ((translation-matrix 0 244))
      (minicommand-frontplate *alu-tool*))))


(defun test-rotation ()
  (with-program ("test")
    (with-tool ((make-instance 'tool :diameter 2 :depth 4 :number 6 :feed-xy 600 :feed-z 240))
      (with-transform ((rotation-matrix 90))
;;	(with-transform ((translation-matrix 100 100))
;;	(drill :x 100 :y 100 :diameter 20)
	(goto-abs :x 100 :y 100)
	(rectangle-inline 20 20)

	))))