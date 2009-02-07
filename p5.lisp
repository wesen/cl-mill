(in-package :gcode)

(defun p5-point (&key x y z)
  (fly-to :x x :y y)
  (with-tool-down ())
  )

(defun p5-line (&key x1 y1 z1 x2 y2 z2)
  (fly-to :x x1 :y y1)
  (with-tool-down ()
    (mill-abs :x x2 :y y2))
  )

(defun p5-triangle (&key x1 y1 x2 y2 x3 y3)
  (fly-to :x x1 :y y1)
  (with-tool-down ()
    (mill-abs :x x2 :y y2)
    (mill-abs :x x3 :y y3)
    (mill-abs :x x1 :y y1))
  )

(defun p5-quad (&key x1 y1 x2 y2 x3 y3 x4 y4)
  (fly-to :x x1 :y y1)
  (with-tool-down ()
    (mill-abs :x x2 :y y2)
    (mill-abs :x x3 :y y3)
    (mill-abs :x x4 :y y4)
    (mill-abs :x x1 :y y1))
  )

(defun p5-rect (&key x y width height)
  (p5-quad :x1 x :y1 y
	   :x2 (+ x width) :y2 y
	   :x3 (+ x width) :y3 (+ y height)
	   :x4 x :y4 (+ y height))
  )

(defun p5-ellipse (&key x y width height)
  (let* ((e-to-b-const 0.2761423749154) ;; (2 / 3 * (sqrt(2) - 1))
	 (offset-x (* e-to-b-const width))
	 (offset-y (* e-to-b-const height))
	 (centre-x (+ x (/ width 2)))
	 (centre-y (+ y (/ height 2))))
    (fly-to :x x :y centre-y)
    (mill-curve
     (curve-to-arcs (list
		     (make-bezier :a (2dp x centre-y)
				  :u (2dp x (- centre-y offset-y))
				  :v (2dp (- centre-x offset-x) y)
				  :b (2dp centre-x y))
		     (make-bezier :a (2dp centre-x y)
				  :u (2dp (+ centre-x offset-x) y)
				  :v (2dp (+ x width) (- centre-y offset-y))
				  :b (2dp (+ x width) centre-y))
		     (make-bezier :a (2dp (+ x width) centre-y)
				  :u (2dp (+ x width) (+ centre-y offset-y))
				  :v (2dp (+ centre-x offset-x) (+ y width))
				  :b (2dp centre-x (+ y width)))
		     (make-bezier :a (2dp centre-x (+ y width))
				  :u (2dp (- centre-x offset-x) (+ y width))
				  :v (2dp x (+ centre-y offset-y))
				  :b (2dp x centre-y)))))))

(defun p5-arc (&key a b c d start stop)
  )

(defun p5-bezier (&key x1 y1 x2 y2 x3 y3 x4 y4)
  (fly-to :x x1 :y y1)
  (mill-curve (bezier-to-arc (make-bezier :a (2dp x1 y1)
					  :u (2dp x2 y2)
					  :v (2dp x3 y3)
					  :b (2dp x4 y4)))))

(defvar *p5-matrixes* nil)

(defmacro with-p5-sketch (() &rest body)
  `(let ((*p5-matrixes* nil)
	 (*current-transform* *current-transform*))
     ,@body))

(defun p5-push-matrix ()
  (push *current-transform* *p5-matrixes*)
  )

(defun p5-pop-matrix ()
  (setf *current-transform* (pop *p5-matrixes*))
  )

(defun p5-translate (&key (tx 0) (ty 0) tz)
  (setf *current-transform* (translation-matrix tx ty))
  )

(defun p5-rotate (&key angle vx vy vz)
  (setf *current-transform* (rotation-matrix angle))
  )

(defun p5-rotate-x (&key angle)
  )

(defun p5-rotate-y (&key angle)
  )

(defun p5-rotate-z (&key angle)
  )

(defun p5-scale (&key s)
  (setf *current-transform* (scaling-matrix s))
  )

(defun p5-program (file &key (scale 0.3))
  (with-program (file)
    (with-named-pass ("p5")
      (with-transform ((scaling-matrix scale))
	(with-tool (*pcb-tool*)
	  (load-file file))))))

(defun p5-random (p1 &optional p2)
  (if p2
      (+ p1 (random (- p2 p1)))
      (random p1)))