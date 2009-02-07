(in-package :gcode)

(defparameter *pcb-tool*
  (make-instance 'tool
		 :diameter 1
		 :number 7
		 :feed-xy 600
		 :feed-z 240
		 :depth 2.2))

(defun pcb (file outfile)
  (program-to-file
   (with-program ("pcb")
     (with-tool (*pcb-tool*)
       (tool-up)
       (home)
       (load-file
	file)))
   outfile :order '("drills" "mill" "bridge-cut")))

#+nil
(defun test-file ()
  (with-program ("file-tool")
    (with-tool (*pcb-tool*)
      (tool-up)
      (home)
      (load-file
       "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand-ioboard.lisp"))))

(defvar *drills*)

(defun add-drill (&key x y diameter)
  (push (list x y diameter) *drills*))

(defun add-via (&key x y diameter)
  (push (list x y diameter) *drills*))


(defun square (x)
  (* x x))

(defun distance (x1 y1 x2 y2)
  (sqrt (+
	 (square (- x1 x2))
	 (square (- y1 y2)))))

(defun nearest-drill (x y drills)
  (let ((distances
	 (sort 
	  (mapcar #'(lambda (drill)
		      (cons (distance x y (first drill) (second drill))
			    (copy-tree drill)))
		  drills)
	  #'< :key #'first)))
    (cdr (first distances))))

(defun drill-direction (x1 y1 x2 y2)
  (let ((direction nil))
    (cond ((> x2 x1) (push :right direction))
	  ((< x2 x1) (push :left direction)))
    (cond ((> y2 y1) (push :up direction))
	  ((< y2 y1) (push :down direction)))
    direction))

(defun sort-drills ()
  *drills*)

(defmacro with-drills (() &rest body)
  `(let ((*drills*))
     ,@body
     (let ((*drills* (sort-drills)))
       (with-named-pass ("drills")
	 (with-tool (*pcb-tool*)
	   (dolist (drill *drills*)
	     (drill :x (first drill)
		    :y (second drill) ;; XXXX invert!!!
		    :diameter (third drill))))))))

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

(defparameter *frontplate-depth* 3.0)

(defun frontplate-element (&key name package x y angle)
  (with-named-pass ("frontplate")
    (cond ((string= package "3FTL06")
	   (drill :x x :y y :diameter 10 :depth *frontplate-depth*))
	  ((string= package "CI-11")
	   (drill :x x :y y :diameter 7.5 :depth *frontplate-depth*))
	  ((string= package "LED5MM")
	   (drill :x x :y y :diameter 5.5 :depth *frontplate-depth*))
	  ((string= package "POWER")
	   (progn
	     (goto-abs :x (- x 7.25) :y (- y 4.25))
	     (rectangle-inline 14.5 8.5 :depth *frontplate-depth*)
	     ))
	  ((string= package "DISPLAY-TEXT-C1624A")
	   (progn (goto-abs :x (- x 32.6) :y (- y 16.9))
		  (rectangle-inline 72 27 :depth *frontplate-depth*))))))

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

(defun test-minicommand-casing ()
  (let ((tool (make-instance 'tool :diameter 2 :depth 1 :number 8 :feed-xy 500 :feed-z 100)))
    
    (with-program ("casing")
      (with-named-pass ("frontplate")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      
      (with-tool (tool)
	(with-transform ((translation-matrix 2.5 -2))
	  (with-transform ((translation-matrix 90 0))
	    (with-transform ((rotation-matrix -90))
	      (with-transform ((translation-matrix 5 3))
		(load-file "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand-ioboard.lisp")))))
	
	(with-named-pass ("umrandung")
	  (goto-abs :x 0 :y -2)
	  (rectangle-inline 95 120 :depth 4)))


	    )))

(defun test-rotation ()
  (with-program ("test")
    (with-tool ((make-instance 'tool :diameter 2 :depth 4 :number 6 :feed-xy 600 :feed-z 240))
      (with-transform ((rotation-matrix 90))
;;	(with-transform ((translation-matrix 100 100))
;;	(drill :x 100 :y 100 :diameter 20)
	(goto-abs :x 100 :y 100)
	(rectangle-inline 20 20)

	))))

(defun optimize-test-file ()
  (let ((program (test-file)))
    (optimize-program-pass program "drills")
    program))