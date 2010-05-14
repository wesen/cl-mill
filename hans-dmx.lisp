(in-package :gcode)

(defun dmx-frontplate-attach ()
	"attach the frontplate to the MDF board"
	(with-program ("dmx-frontplate-attach")	
		(with-tool (*mdf-tool-2mm*)
			(with-named-pass ("frontplate-attach")
				(with-tool (*mdf-tool-2mm*)
					(drill :x 0 :y 0 :diameter 5 :depth 4)
					(drill :x 0 :y 32 :diameter 5 :depth 4)
					(drill :x 423 :y 0 :diameter 5 :depth 4)
					(drill :x 423 :y 32 :diameter 5 :depth 4))))))

(defparameter *dmx-depth* 3.5)

(defun dmx-buchse (&key x y)
		(drill :x x
					 :y y
					 :diameter 3.1 :depth *dmx-depth*)
		(drill :x (+ x 17)
					 :y (+ y 26)
					 :diameter 3.1 :depth *dmx-depth*)
		
		(drill :x (+ x (/ 17 2))
					 :y (+ y (/ 26 2))
					 :diameter 26 :depth *dmx-depth*))

(defun usb-buchse (&key x y)
	(drill :x x
				 :y y
				 :diameter 3.1 :depth *dmx-depth*)
	(drill :x (+ x 19)
				 :y (+ y 24)
				 :diameter 3.1 :depth *dmx-depth*)
	(drill :x (+ x (/ 19 2))
				 :y (+ y (/ 24 2))
				 :diameter 23.6 :depth *dmx-depth*))

(defparameter *drawing-tool*
	(make-instance 'tool
								 :diameter 0.01
								 :depth *dmx-depth*
								 :number 99))
								 

(defun dmx-frontplate ()
	"mill backside of DMX frontplate"
	(with-program ("dmx-frontplate")
		(with-tool (*alu-tool*)

			(with-named-pass ("frontplate-backside")
				(with-tool (*alu-tool*)
					(let ((spacing 30)
								(start-offset 22)
								(usb-offset 22))
						
					(loop for i from 0 below 12
						 do (dmx-buchse :x (+ start-offset (* i spacing))
														:y (/ (- 32 (+ 26 (/ 3.1 2))) 2)))

						(usb-buchse :x (+ usb-offset (* 12 spacing) 2)
												:y (/ (- 32 (+ 23.6 3.1)) 2))
					)))

			(with-named-pass ("frontplate-attach")
				(with-tool (*mdf-tool-2mm*)
					(drill :x 0 :y 0 :diameter 5 :depth *dmx-depth*)
					(drill :x 0 :y 32 :diameter 5 :depth *dmx-depth*)
					(drill :x 423 :y 0 :diameter 5 :depth *dmx-depth*)
					(drill :x 423 :y 32 :diameter 5 :depth *dmx-depth*))))))

									 
	
