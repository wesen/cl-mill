(in-package :Gcode)

(defun monojoystick-frontplate (tool)
	(with-program ("monojoystick")
      
		(with-named-pass ("frontplate")
			(with-tool (tool)
				(goto-abs :x 0 :y 0)
				(goto-abs :z *fly-height*)))
      
		(with-named-pass ("drills")
			(with-tool (tool)
				(goto-abs :x 0 :y 0)
				(goto-abs :z *fly-height*)))
      
      
		(with-named-pass ("mill")
			(with-tool (tool)
				(goto-abs :x 0 :y 0)
				(goto-abs :z *fly-height*)))
      
      
		(with-named-pass ("bridge-cut")
			(with-tool (tool)
				(goto-abs :x 0 :y 0)
				(goto-abs :z *fly-height*)))
      
    (with-tool (tool)
      (with-transform ((translation-matrix 2.8 -0.75))
				(with-transform ((translation-matrix 90 0))
					(with-transform ((rotation-matrix -90))
						(with-transform ((translation-matrix 5 3))
							(load-file "/Users/manuel/weptech/monojoystick-ioboard.lisp")
							(with-named-pass ("frontplate")
								(with-tool (tool)
									(frontplate-element :name "JOYSTICK" :package "JOYSTICK" :x 54.85 :y 12.48000 :angle 0.0)
									(frontplate-element :name "U$2" :package "DISPLAY-TEXT-C1624A" :x 51.640000 :y 65.500000 :angle 0.000000)))
	      
							)))))))
  
  
