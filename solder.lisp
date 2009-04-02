(in-package :gcode)

;; loet hilfen

(defparameter *dick-holz-tool*
  (make-instance 'tool
		 :number 16
		 :diameter 3
		 :depth 3))

(defun loethilfe ()
  (with-program ("loethilfe")
    (with-named-pass ("mill")
      (with-tool (*dick-holz-tool*)
	(goto-abs :x 0 :y 0)
	(goto-abs :z *fly-height*)
	(fly-to :x 10 :y 10)
	(rectangle-mill  50 50 :depth 3)
	(fly-to :x 10 :y 70)
	(rectangle-mill 50 50 :depth 3)
	(fly-to :x 10 :y 130)
	(rectangle-mill 50 50 :depth 3)
	(fly-to :x 10 :y 190)
	(rectangle-mill 50 50 :depth 3)
	(fly-to :x 10 :y 250)
	(rectangle-mill 50 50 :depth 3)
	
	(drill :x 100 :y 100 :diameter 6 :depth 6)
	(drill :x 126.76 :y 100 :diameter 6 :depth 6)
	(drill :x 154.52 :y 100 :diameter 6 :depth 6)
	(drill :x 181.80 :y 100 :diameter 6 :depth 6)))))
						