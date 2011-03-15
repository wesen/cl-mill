(in-package :gcode)

(defparameter *gravier-tool-ungi*
	(make-instance 'tool :diameter 0 :depth 1 :number 19 :feed-xy 500 :feed-z 100))

(defun test-ungi-1 ()
	(with-program ("ungi-test")
		(with-named-pass ("test")
			(with-tool (*gravier-tool-ungi*)
				(goto-abs :x 0 :y 0)
				(rectangle 20 20)))))


;; 5.6 cm radius

(defun test-ungi-svg (svg)
	(with-program ("ungi-svg-test")
		(with-named-pass ("svg")
			(with-tool (*gravier-tool-ungi*)
				(with-transform ((rotation-matrix 90))
					(with-transform ((scaling-matrix 0.25))
						(svg-pass svg)))))))

