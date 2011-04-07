(in-package :gcode)

(defparameter *alu-tool-top*
  ;; FUER FRONTPLATTEN
  (make-instance 'tool :diameter 2 :depth 1 :number 11 :feed-xy 500 :feed-z 100))

(defun minicommand-casing-fix-blau ()
  (let ((tool *alu-tool-top*)
	(*frontplate-depth* 8.4))
    
    (with-program ("fix")
      (with-named-pass ("fix")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)

          (repeat-for-depth (24.4)
                            (rectangle 1 80)))))))
        
