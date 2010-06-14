(in-package :gcode)

(defparameter *gravier-tool-ungi*
	(make-instance 'tool :diameter 0 :depth 1 :number 19 :feed-xy 500 :feed-z 100))

(defun test-ungi-1 ()
	(with-program ("ungi-test")
		(with-named-pass ("test")
			(with-tool (*gravier-tool-ungi*)
				(goto-abs :x 0 :y 0)
				(rectangle 20 20)))))

(defun test-ungi-svg ()
	(with-program ("ungi-svg-test")
		(with-named-pass ("svg")
			(with-tool (*gravier-tool-ungi*)
				(svg-pass "/Users/manuel/drawing-2.svg")))))
			