(in-package :gcode)

(defun bug-rotation-program ()
  (with-program ("bug")
    (with-tool (*pcb-tool*)
      (goto-abs :x 30 :y 30)
      (format t "current-x ~A orig-x ~A current-y ~A orig-y ~A~%"
	      (current-x) (orig-current-x)
	      (current-y) (orig-current-y))
      (with-transform ((rotation-matrix 45))
      (format t "current-x ~A orig-x ~A current-y ~A orig-y ~A~%"
	      (current-x) (orig-current-x)
	      (current-y) (orig-current-y))
	
	(with-tool-down ()
	  (format t "current-x ~A orig-x ~A current-y ~A orig-y ~A~%"
		  (current-x) (orig-current-x)
		  (current-y) (orig-current-y))
	  
	  (mill-rel :x 10)
	  (mill-rel :y 10)
	  (mill-rel :x -10 :y -10))))))