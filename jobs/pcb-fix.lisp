(in-package :gcode)

(defparameter *epoxy-tool*
  (make-instance 'tool
		 :diameter 0.3
		 :number 13
		 :depth 1))

(defun pcb-fix ()
  (let ((*fly-height* 8))
    (with-program ("pcb-fix")
      (goto-abs :z *fly-height*)
      (with-tool (*epoxy-tool*)
	(with-named-pass ("mill")
	  (home)
	  (goto-abs :x 24.9 :y 3)
	  (with-tool-down (0.8)
	    (mill-rel :x -9)
	    (mill-rel :y 11)
	    
	    (mill-rel :x 6)
	    (mill-rel :y 8)
	    (mill-rel :x 14)
	    (mill-rel :y -14)
	    (mill-rel :x -11)
	    (mill-rel :y -5)
	    ))))))
  