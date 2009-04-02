(in-package :gcode)

(defparameter *cupe-tool*
  (make-instance 'tool
		 :diameter 3 ;; diameter vom tool
		 :number 14  ;; nummer im cnc kontrollprogramm
		 :depth 3    ;; maximale eintauchstiefe vom tool
		 ))

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
(program-to-file (cupe-shizzle) "/users/manuel/public/nc/test.nc" :order '("mill" "bridge"))
(program-to-pdf (cupe-shizzle) "/users/manuel/public/nc/test.pdf" :order '("mill"))

;; opcodes.lisp -> alles was direkt mit gcode zu tun hat - primitiven
;; shapes.lisp -> (DRILL) (ARC-CW-ABS) (ARC-CCW-ABS) (MILL-ABS)