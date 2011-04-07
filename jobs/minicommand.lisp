(in-package :gcode)

;; CI-11 7 mm radius
;; 3FTL06 10 mm radius

(defvar *frontplate-program*)
(defvar *frontplate-elements*)

(defparameter *frontplate-depth* 2.8)
(defvar *frontplate-top* t)
(defvar *frontplate-side* nil)

;; proto

(defun frontplate-element (&key name package x y angle)
  (with-named-pass ("frontplate")
    (cond ((or (string= package "3FTL06")
	       (string= package "3FTL06-LED"))
	   (when *frontplate-top*

	     (with-named-pass ("test")
	       (drill :x x :y y :diameter 2 :depth *frontplate-depth*))
	     
	     ;; real one
	     (drill :x x :y y :diameter 11.3 :depth *frontplate-depth*)
	     ))
	  
	  ((string= package "JOYSTICK")
	   (drill :x x :y y :diameter 23 :depth *frontplate-depth*))
	  
	  ((string= package "CI-11")
	   (when *frontplate-top*
	     (drill :x x :y y :diameter 7.5 :depth *frontplate-depth*)
	     (with-named-pass ("test")
	       (drill :x x :y y :diameter 2 :depth 0.5))))
	  
	  ((string= package "LED5MM")
	   (when *frontplate-top*
	     (drill :x x :y y :diameter 5.5 :depth *frontplate-depth*)))

	  ((string= package "POWER")

	   (when *frontplate-top*
	     ;; orig
	     (progn
	       (goto-abs :x (- x 7.35) :y (- y 3.85))
	       (rectangle-inline 14.2 7.7 :depth *frontplate-depth*)
	       )))

	  ((string= package "MAB5SH")
	   (when *frontplate-side*
	     #-debug
	     (drill :x 11.5 :y (- x 1.0) :diameter 18.5 :depth *frontplate-depth*)
	     
	     (with-named-pass ("test")
	       (drill :x 11.5 :y (- x 1.0) :diameter 2 :depth 1.5))
	     (with-named-pass ("test3")
               (with-tool (*alu-tool*)
                 (drill :x 11.5 :y (- x 1.0) :diameter 8 :depth 1.5)))
	     (with-named-pass ("test2")
	       (drill :x 11.5 :y (- x 1.0) :diameter 18.5 :depth 1.5))
	     ))
	  
	  ((string= package "DCJ0202")
	   (when *frontplate-side*
	     (with-named-pass ("test")
	       nil)

	     (with-named-pass ("test2")
	       (progn
		 (goto-abs :x 1.2 :y (+ y 5))
		 (rectangle-inline 11 9.5 :depth 1.5)))

	     #-debug
	     (progn
	       (goto-abs :x 1.7 :y (+ y 5.3))
	       (rectangle-inline 10.7 8.6 :depth *frontplate-depth*))))
	  
	  ((string= package "DISPLAY-TEXT-C1624A")

	   (when *frontplate-top*
             (with-named-pass ("frontplate")
               (with-tool (*alu-tool*)
                 (progn (goto-abs :x (- x 32.7) :y (- y 17))
                        (rectangle-inline 71.7 26.7 :depth (+ 0.0 *frontplate-depth*))))))))))


(defparameter *alu-tool-top*
  ;; FUER FRONTPLATTEN
  (make-instance 'tool :diameter 2 :depth 0.2 :number 11 :feed-xy 500 :feed-z 100))

(defparameter *alu-tool*
  (make-instance 'tool :diameter 2 :depth 0.2 :number 11 :feed-xy 500 :feed-z 100))

;; FUER FROTPLATTEN
(defparameter *alu-tool*
  (make-instance 'tool :diameter 2 :depth 0.3 :number 11 :feed-xy 500 :feed-z 100))

(defun minicommand-casing-side-top ()
  (let ((tool *alu-tool-top*)
	(*frontplate-depth* 3.4))
    
    (with-program ("casing")
      (with-named-pass ("umrandung")
	(goto-abs :x 0 :y 0)
	(rectangle 30 119))

      (with-named-pass ("frontplate")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      (with-named-pass ("test")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      (with-named-pass ("test2")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      (with-named-pass ("test3")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))
      
      (with-named-pass ("mill")
	(with-tool (*alu-tool-top*)
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

  (with-named-pass ("test")
    (with-tool (tool)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)))

  (with-tool (tool)
    (with-transform ((translation-matrix 2.8 -0.75))
      (with-transform ((translation-matrix 90 0))
	(with-transform ((rotation-matrix -90))
	  (with-transform ((translation-matrix 5 3))
	    (load-file "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand-ioboard.lisp"))))))

  (with-named-pass ("debug")
    (with-tool (tool)
      (goto-abs :x 0 :y 0)
      (rectangle 94 119)))
  
  #+nil
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


(defun single-minicommand ()
  (with-program ("casing")
    (minicommand-frontplate *alu-tool*)))

