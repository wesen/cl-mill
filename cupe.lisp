(in-package :gcode)

(defparameter *cupe-tool*
  (make-instance 'tool
		 :diameter 0.3 ;; diameter vom tool
		 :number 14  ;; nummer im cnc kontrollprogramm
		 :depth 4    ;; maximale eintauchstiefe vom tool
		 ))

#+nil
(defun cupe-shizzle ()
  ;; viereck von 10, 10 -> 50, 50
  (with-program ("cupe-shizzle")
    (with-named-pass ("viereck")
      (goto-abs :x 0 :y 0)
      (spindle-on)
      (goto-abs :z 1.5)
      (goto-abs :x 10 :y 10)
      (let ((*current-tool* *cupe-tool*))
	(with-named-pass ("befestigung")
	  (drill :x 8 :y 8 :diameter 5 :depth 8))
	(with-tool-down (2)	
	  (let ((x1 (orig-current-x))
		(y1 (orig-current-y)))
	    (with-named-pass ("bridge")
	      (with-tool-down (1)
		(fly-to :x x1 :y y1)
		(mill-abs :x 15 :y y1))))

	  (fly-to :x 15)
	  (mill-abs :x 50 :y 10)
	  (mill-abs :x 50 :y 50)

	  (let ((x1 (orig-current-x))
		(y1 (orig-current-y)))
	    (with-named-pass ("bridge")
	      (with-tool-down (1)
		(fly-to :x x1 :y y1)
		(mill-abs :x 45 :y y1))))
	  
	  (fly-to :x 45)
	  (mill-abs :x 10 :y 50)
	  (mill-abs :x 10 :y 10))))))

;; NC code genereien
;;(program-to-file (cupe-shizzle) "/users/manuel/public/nc/test.nc" :order '("mill" "bridge"))
;(program-to-pdf (cupe-shizzle) "/users/manuel/public/nc/test.pdf" :order '("mill"))

;; opcodes.lisp -> alles was direkt mit gcode zu tun hat - primitiven
;; shapes.lisp -> (DRILL) (ARC-CW-ABS) (ARC-CCW-ABS) (MILL-ABS)

(defun holz-test ()
  (with-program ("bohrung")
      (with-named-pass ("drills")
	(with-tool (*cupe-tool*)
;;	  (drill :x 20 :y 10 :diameter 5 :depth 1)
;;	  (drill :x 465 :y 10 :diameter 5 :depth 1)
;;	  (drill :x 465 :y 400 :diameter 5 :depth 1)
	  (drill :x 20 :y 400 :diameter 5 :depth 1)

	  

	  ))))

(defun holz-test2 ()
  (with-program ("bohrung")
      (with-named-pass ("drills")
	(with-tool (*cupe-tool*)
	  (drill :x 20 :y 10 :diameter 5 :depth 16.5)

	  

	  ))))

;;; AB HIER

(defun m+ (&rest points)
  (apply #'mapcar #'+ points))

(defun make-point (x y)
  (list x y))

(defmethod x ((p cons))
  (first p))

(defmethod y ((p cons))
  (second p))

(defmethod m* ((p1 cons) (p2 cons))
  (+ (* (x p1) (x p2))
     (* (y p1) (y p2))))

(defmethod m* ((p1 cons) (n number))
  (mapcar (lambda (foo)
            (* foo n))
          p1))

(defmethod m* ((n number) (p1 cons))
  (m* p1 n))

(defun evaluate-arc (center radius angle)
  (m+ center
      (m* radius (make-point (cos angle)
                             (sin angle)))))

(defparameter *aussen-radius* 43.5)
(defparameter *innen-radius* (- *aussen-radius* 4.7))
(defparameter *offset* 2.1)
(defparameter *led-loch-radius* 0.3)
(defparameter *led-beinchen-radius* 0.175)
(defparameter *schraubenloch-radius* 0.301)
(defparameter *profilschraubenloch-radius* 0.401)

(defun goto-cupe (v)
  (goto-abs :x (x v) :y (y v)))

(defun unteres-teil-program ()
  (with-program ("bla")
    (with-transform ((scaling-matrix 10))
      (unteres-teil))))

(defun oberes-teil-program ()
  (with-program ("bla")
    (with-transform ((scaling-matrix 10))
      (oberes-teil))))



(defparameter *rillen-tiefe* 4)   ;; 4
(defparameter *lueftung-tiefe* 4) ;; 4
(defparameter *holz-tiefe* 16.5)     ;; 16
(defparameter *led-tiefe* 10)      ;; 14
(defparameter *kabelkanal-tiefe* 8)


(defun unteres-teil ()
  (let ((tool-radius (/ (slot-value *cupe-tool* 'gcode::diameter) 2))
        (gcode::*current-tool* *cupe-tool*))
      (with-named-pass ("aussenkante")
      (with-tool (*cupe-tool*)
	(let* ((center (make-point 0 0))
	       (*aussen-radius* (+ *aussen-radius* tool-radius))
	       (*innen-radius* (- *innen-radius* tool-radius))
	       (*offset* (- *offset* tool-radius))
	       (p1 (make-point (* *aussen-radius* (cos (asin (/ *offset* *aussen-radius*))))
			       *offset*))
	       (p2 (make-point *offset*
			       (* *aussen-radius* (sin (acos (/ *offset* *aussen-radius*))))))
	       (p3 (make-point *offset*
			       (* *innen-radius* (sin (acos (/ *offset* *innen-radius*))))))
	       (p4 (make-point (* *innen-radius* (cos (asin (/ *offset* *innen-radius*))))
			       *offset*)))
	  (format t "p1 before: ~A~%" p1)
	  (let ((p1 (make-point (+ -0.1 (x p1)) (+ 0.1 (y p1))))
		(p2 (make-point (+ 0.1 (x p2)) (+ -0.1 (y p2))))
		(p3 (make-point (+ 0.1 (x p3)) (+ 0.1 (y p3))))
		(p4 (make-point (+ 0.1 (x p4)) (+ 0.1 (y p4)))))
	    (format t "p1: ~A~%" p1)
	    (goto-cupe p1)
	    (repeat-for-depth (3)
			      (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))
			      (mill-abs :x (x p3) :y (y p3))
			      (arc-cw-abs :x (x p4) :y (y p4) :i (x center) :j (y center))
			      (mill-abs :x (x p1) :y (y p1))))

	  (let ((p1 (make-point (+ 0.1 (x p1)) (+ -0.1 (y p1))))
		(p2 (make-point (+ -0.1 (x p2)) (+ 0.1 (y p2))))
		(p3 (make-point (+ -0.1 (x p3)) (+ -0.1 (y p3))))
		(p4 (make-point (+ -0.1 (x p4)) (+ -0.1 (y p4)))))
	    (goto-cupe p1)
	    (repeat-for-depth (3)
			      (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))
			      (mill-abs :x (x p3) :y (y p3))
			      (arc-cw-abs :x (x p4) :y (y p4) :i (x center) :j (y center))
			      (mill-abs :x (x p1) :y (y p1))))
	  
	  (goto-cupe p1)
	  (repeat-for-depth (*holz-tiefe*)
			    (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))
			    (mill-abs :x (x p3) :y (y p3))
			    (arc-cw-abs :x (x p4) :y (y p4) :i (x center) :j (y center))
			    (mill-abs :x (x p1) :y (y p1))))))
      
      (with-named-pass ("rille1")
	(with-tool (*cupe-tool*)
          (loop for radius from (- *aussen-radius* 1.4 tool-radius)
                           downto (+ (- *aussen-radius* 1.4 1.2) tool-radius)
                           by (- (* tool-radius 2) 0.05)
                do (let* ((center (make-point 0 0))
                          (p1 (make-point (* radius (cos (asin (/ *offset* radius))))
                                          *offset*))
                          (p2 (make-point *offset*
                                          (* radius (sin (acos (/ *offset* radius)))))))
                     (goto-cupe p1)
                     (repeat-for-depth (*kabelkanal-tiefe*)
		       (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center)))))
          (let* ((center (make-point 0 0))
                 (radius (+ (- *aussen-radius* 1.4 1.2) tool-radius))
                 (p1 (make-point (* radius (cos (asin (/ *offset* radius))))
                                          *offset*))
                          (p2 (make-point *offset*
                                          (* radius (sin (acos (/ *offset* radius)))))))
                     (goto-cupe p1)
                     (repeat-for-depth (*kabelkanal-tiefe*)
                       (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))))))

      (with-named-pass ("profilschrauben")
	(with-tool (*cupe-tool*)
        (let ((radius (- *aussen-radius* 2.0)))
          (loop for winkel in (mapcar (lambda (x)
                                        (* x pi (/ 180)))
                                      '(15 35 55 75))
                do (let ((punkt (evaluate-arc (make-point 0 0) radius winkel)))
                     (goto-cupe punkt)
                     (drill :x (x punkt) :y (y punkt)
                            :diameter (* *profilschraubenloch-radius* 2)
                            :depth *holz-tiefe*))))))

      (with-named-pass ("lueftung")
	(with-tool (*cupe-tool*)
        (let* ((tool-radius (/ (slot-value *cupe-tool* 'gcode::diameter) 2))
	       (radius1 (- *innen-radius* tool-radius))
	       (radius2 (+ *innen-radius* 1.9 0.3 tool-radius))
              (delta 0.1))
          (loop for winkel
                from (asin (/ (+ *offset* 1.0) radius1))
                to (acos (/ (+ *offset* 1.0) radius1))
                by delta
                for i from 0
                do (let ((p1 (evaluate-arc (make-point 0 0) radius1 winkel))
                         (p2 (evaluate-arc (make-point 0 0) radius2 winkel)))
                 
                     (goto-cupe p1)
                     (repeat-for-depth (*kabelkanal-tiefe*) ;;lueftung
                       (mill-abs :x (x p2) :y (y p2))))))))
      
      (with-named-pass ("schrauben")
	(with-tool (*cupe-tool*)
        (let ((radius (- *aussen-radius* 0.8)))
          (loop for winkel in (mapcar (lambda (x)
                                        (* x pi (/ 180)))
                                      '(10 30 60 80))
                do (let ((punkt (evaluate-arc (make-point 0 0) radius winkel)))
                  
                     (goto-cupe punkt)
                     (drill :x (x punkt) :y (y punkt)
                            :diameter (* *schraubenloch-radius* 2)
                            :depth *holz-tiefe*))))))))

;;todo: brett-tiefe?
(defun oberes-teil ()
  (let ((tool-radius (/ (slot-value *cupe-tool* 'gcode::diameter) 2))
        (gcode::*current-tool* *cupe-tool*))
    (with-named-pass ("aussenkante")
      (with-tool (*cupe-tool*)
	(let* ((center (make-point 0 0))
	       (*aussen-radius* (+ *aussen-radius* tool-radius))
	       (*innen-radius* (- *innen-radius* tool-radius))
	       (*offset* (- *offset* tool-radius))
	       (p1 (make-point (* *aussen-radius* (cos (asin (/ *offset* *aussen-radius*))))
			       *offset*))
	       (p2 (make-point *offset*
			       (* *aussen-radius* (sin (acos (/ *offset* *aussen-radius*))))))
	       (p3 (make-point *offset*
			       (* *innen-radius* (sin (acos (/ *offset* *innen-radius*))))))
	       (p4 (make-point (* *innen-radius* (cos (asin (/ *offset* *innen-radius*))))
			       *offset*)))
	  (format t "p1 before: ~A~%" p1)
	  (let ((p1 (make-point (+ -0.1 (x p1)) (+ 0.1 (y p1))))
		(p2 (make-point (+ 0.1 (x p2)) (+ -0.1 (y p2))))
		(p3 (make-point (+ 0.1 (x p3)) (+ 0.1 (y p3))))
		(p4 (make-point (+ 0.1 (x p4)) (+ 0.1 (y p4)))))
	    (format t "p1: ~A~%" p1)
	    (goto-cupe p1)
	    (repeat-for-depth (3)
			      (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))
			      (mill-abs :x (x p3) :y (y p3))
			      (arc-cw-abs :x (x p4) :y (y p4) :i (x center) :j (y center))
			      (mill-abs :x (x p1) :y (y p1))))

	  (let ((p1 (make-point (+ 0.1 (x p1)) (+ -0.1 (y p1))))
		(p2 (make-point (+ -0.1 (x p2)) (+ 0.1 (y p2))))
		(p3 (make-point (+ -0.1 (x p3)) (+ -0.1 (y p3))))
		(p4 (make-point (+ -0.1 (x p4)) (+ -0.1 (y p4)))))
	    (goto-cupe p1)
	    (repeat-for-depth (3)
			      (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))
			      (mill-abs :x (x p3) :y (y p3))
			      (arc-cw-abs :x (x p4) :y (y p4) :i (x center) :j (y center))
			      (mill-abs :x (x p1) :y (y p1))))
	  
	  (goto-cupe p1)
	  (repeat-for-depth (*holz-tiefe*)
			    (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))
			    (mill-abs :x (x p3) :y (y p3))
			    (arc-cw-abs :x (x p4) :y (y p4) :i (x center) :j (y center))
			    (mill-abs :x (x p1) :y (y p1))))))
    
    (with-named-pass ("rille1")
      (with-tool (*cupe-tool*)
	(loop for radius from (- *aussen-radius* 1.65 tool-radius)
	   downto (+ (- *aussen-radius* 1.65 0.9) tool-radius)
	   by (- (* tool-radius 2) 0.05)
	   do (let* ((center (make-point 0 0))
		     (p1 (make-point (* radius (cos (asin (/ *offset* radius))))
				     *offset*))
		     (p2 (make-point *offset*
				     (* radius (sin (acos (/ *offset* radius)))))))
		(goto-cupe p1)
		(repeat-for-depth (*rillen-tiefe*)
				  (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center)))))
	(let* ((center (make-point 0 0))
	       (radius (+ (- *aussen-radius* 1.65 0.9) tool-radius))
	       (p1 (make-point (* radius (cos (asin (/ *offset* radius))))
			       *offset*))
	       (p2 (make-point *offset*
			       (* radius (sin (acos (/ *offset* radius)))))))
	  (goto-cupe p1)
	  (repeat-for-depth (*rillen-tiefe*)
			    (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))))))
      
    (with-named-pass ("rille2")
      (with-tool (*cupe-tool*)
	(loop for radius from (+ *innen-radius* 0.4 tool-radius)
	   to  (+ *innen-radius* 0.4 tool-radius 0.9)
	   by (- (* tool-radius 2) 0.05)
	   do (let* ((center (make-point 0 0))
		     (p1 (make-point (* radius (cos (asin (/ *offset* radius))))
				     *offset*))
		     (p2 (make-point *offset*
				     (* radius (sin (acos (/ *offset* radius)))))))
		(goto-cupe p1)
		(repeat-for-depth (*rillen-tiefe*)
				  (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center)))))
	(let* ((center (make-point 0 0))
	       (radius (+ *innen-radius* 0.4 tool-radius 0.9))
	       (p1 (make-point (* radius (cos (asin (/ *offset* radius))))
			       *offset*))
	       (p2 (make-point *offset*
			       (* radius (sin (acos (/ *offset* radius)))))))
	  (goto-cupe p1)
	  (repeat-for-depth (*rillen-tiefe*)
			    (arc-ccw-abs :x (x p2) :y (y p2) :i (x center) :j (y center))))))
      
    (with-named-pass ("loecher")
      (with-tool (*cupe-tool*)
	(let ((radius  (- *aussen-radius* 1.65 0.45))
	      (delta 0.02))
	  (loop for winkel
	     from (asin (/ (+ *offset* *led-loch-radius* 0.1) radius))
	     to (acos (/ (+ *offset* *led-loch-radius* 0.1) radius))
	     by delta
	     for i from 0
	     do (let ((punkt (evaluate-arc (make-point 0 0) radius winkel))
		      (p1 (evaluate-arc (make-point 0 0) (+ (- radius 0.3 0.15) tool-radius) winkel))
		      (p2 (evaluate-arc (make-point 0 0) (- (+ radius 0.3 0.15) tool-radius) winkel)))
		  (format t "loch nummer ~a.~%" i)
		  (goto-cupe punkt)
		  (drill :x (x punkt) :y (y punkt)
			 :diameter (* *led-loch-radius* 2)
			 :depth (+ *led-tiefe* *rillen-tiefe*))
	;	  (goto :x (x punkt) :y (y punkt) :z (+ *led-tiefe* *rillen-tiefe*))
		  (drill :x (x punkt) :y (y punkt)
			 :diameter (* *led-beinchen-radius* 2)
			 :depth *holz-tiefe*)
	;

		  (goto :x (x punkt) :y (y punkt) :z *rillen-tiefe*)
	;	  (goto :x (x p1) :y (y p1) :z *rillen-tiefe*)
		  (goto-cupe p1)
		  (repeat-for-depth ((+ *lueftung-tiefe* *rillen-tiefe*)) ;;lueftung
				    (mill-abs :x (x p2) :y (y p2))))))))

    (with-named-pass ("schrauben")
      (with-tool (*cupe-tool*)
	(let ((radius (- *aussen-radius* 0.8)))
	  (loop for winkel in (mapcar (lambda (x)
					(* x pi (/ 180)))
				      '(10 30 60 80))
	     do (let ((punkt (evaluate-arc (make-point 0 0) radius winkel)))
                  
		  (goto-cupe punkt)
		  (drill :x (x punkt) :y (y punkt)
			 :diameter (* *schraubenloch-radius* 2)
			 :depth *holz-tiefe*))))))))

;; NC code genereien
					;(program-to-file (cupe-shizzle) "/users/manuel/public/nc/test.nc" :order '("mill" "bridge"))
					;(program-to-pdf (cupe-shizzle) "/users/manuel/public/nc/test.pdf" :order '("mill"))

;; opcodes.lisp -> alles was direkt mit gcode zu tun hat - primitiven
;; shapes.lisp -> (DRILL) (ARC-CW-ABS) (ARC-CCW-ABS) (MILL-ABS)