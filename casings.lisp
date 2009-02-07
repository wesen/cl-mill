(in-package :gcode)

(defun hammond-29830-psla ()
  (with-program ("hammond-29830")
    (with-tool ((make-instance 'tool
			       :diameter 2
			       :number 6
			       :feed-xy 600
			       :feed-z 240
			       :depth 3))

      (spindle-on)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)

      (with-named-pass ("outline")
	(goto-abs :x 0 :y 0)
	(rectangle-inline 95 120 :depth 10)
	(goto-abs :x 2 :y 2)
	(rectangle-inline (- 95 4) (- 120 4) :depth 10))

      (with-named-pass ("fix")

	(goto-abs :x 0 :y 0)
	(goto-abs :y 119.2 :x 0)
	(with-tool-down (3)
	  (mill-abs :y 119.2 :x 95))
	(with-tool-down (6)
	  (mill-abs :y 119.2 :x 0))
	(with-tool-down (10)
	  (mill-abs :y 119.2 :x 95))
	
	(goto-abs :y (- 120 9) :x 95)
	(with-tool-down (3)
	  (mill-abs :y (- 120 9) :x 86))
	(with-tool-down (6)
	  (mill-abs :y (- 120 9) :x 95))
	(goto-abs :y (- 120 9) :x 0)
	(with-tool-down (3)
	  (mill-abs :y (- 120 9) :x 9))
	(with-tool-down (6)
	  (mill-abs :y (- 120 9) :x 0))

	(goto-abs :x 0 :y 0)
	(rectangle-inline (- 95 4) (- 120 4) :depth 1)

	
      )
      
      (with-named-pass ("inner-rectangles")

	(goto-abs :y 0         :x 0)
	(rectangle-mill 9 9 :depth 6)

	(goto-abs :y (- 120 9) :x 0)
	(rectangle-mill 9 9 :depth 6)

	(goto-abs :y (- 120 9) :x (- 95 9))
	(rectangle-mill 9 9 :depth 6)

	(goto-abs :y 0         :x (- 95 9))
	(rectangle-mill 9 9 :depth 6))

      (with-named-pass ("drills")
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
	
	(drill :y 4.5         :x 4.5        :diameter 3 :depth 16)
	(drill :y (- 120 4.5) :x 4.5        :diameter 3 :depth 16)
	(drill :y (- 120 4.5) :x (- 95 4.5) :diameter 3 :depth 16) 
	(drill :y 4.5         :x (- 95 4.5) :diameter 3 :depth 16)))))
  

;; X, Y coordnates test brett
;; 42.9, 20.6 -> 2, 2