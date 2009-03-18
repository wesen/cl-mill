(in-package :gcode)

(defun offset-line (line offset)
  (let* ((normal (vector-normal line))
	 (offset-normal (point-* normal offset)))
    (make-line :a (point-+ (line-a line) offset-normal)
	       :b (point-+ (line-b line) offset-normal))))

(defun offset-arc (arc offset)
  (with-slots (a b centre direction) arc
    (let ((vb (normalize-vector (make-line :a centre :b b)))
	  (va (normalize-vector (make-line :a centre :b a)))
	  (off2 (case direction
		 (:cw  offset)
		 (:ccw (- offset))))
	  (l1 (arc-radius arc)))
      (if (and (< off2 0)
	       (>= (abs off2) l1))
	  (offset-line (make-line :a a :b b) offset)
	  (make-arc :centre centre :direction direction
		    :a (point-+ a (point-* va off2))
		    :b (point-+ b (point-* vb off2)))))))

(defun point-= (p1 p2)
  (and (= (2d-point-x p1) (2d-point-x p2))
       (= (2d-point-y p1) (2d-point-y p2))))

(defun line-= (l1 l2)
  (and (point-= (line-a l1) (line-a l2))
       (point-= (line-b l1) (line-b l2))))

(defun point-epsilon= (p1 p2)
  (and (epsilon-= (2d-point-x p1) (2d-point-x p2))
       (epsilon-= (2d-point-y p1) (2d-point-y p2))))


(defun arc-= (a1 a2)
  (and (point-= (arc-centre a1) (arc-centre a2))
       (if (eq (arc-direction a1) (arc-direction a2))
	   (and (point-= (arc-a a1) (arc-a a2))
		(point-= (arc-b a1) (arc-b a2)))
	   (and (point-= (arc-a a1) (arc-b a2))
		(point-= (arc-b a1) (arc-a a2))))))
       
(deftest :offset-arc "Offset cw arc outside"
  (let ((arc (make-arc :centre (2dp 100 100)
		       :a (2dp 90 100)
		       :b (2dp 100 90)
		       :direction :cw)))
    (test-assert
     (arc-= (offset-arc arc 5)
	    (make-arc :centre (2dp 100 100)
		      :a (2dp 85 100)
		      :b (2dp 100 85)
		      :direction :cw)))

    (test-assert
     (arc-= (offset-arc arc 10)
	    (make-arc :centre (2dp 100 100)
		      :a (2dp 80 100)
		      :b (2dp 100 80)
		      :direction :cw)))

    (test-assert
     (arc-= (offset-arc arc 20)
	    (make-arc :centre (2dp 100 100)
		      :a (2dp 70 100)
		      :b (2dp 100 70)
		      :direction :cw)))
    ))

(deftest :offset-arc "Offset ccw arc outside"
  (let ((arc (make-arc :centre (2dp 100 100)
		       :a (2dp 90 100)
		       :b (2dp 100 90)
		       :direction :ccw)))
    (test-assert
     (arc-= (offset-arc arc 5)
	    (make-arc :centre (2dp 100 100)
		      :a (2dp 95 100)
		      :b (2dp 100 95)
		      :direction :ccw)))

    (test-assert
     (null (offset-arc arc 10)))

    (test-assert
     (null (offset-arc arc 20)))
    ))


(defun wrap-around (curve)
  (concatenate 'list curve (list (first curve))))

(defgeneric intersection-object (obj1 obj2))

(defstruct long-line
  a b)

(defmethod intersection-object ((l1 long-line) (l2 line))
  (let ((res (line-intersection (long-line-a l1) (long-line-b l1) (line-a l2) (line-b l2))))
    (cond ((null res) nil)
	  ((eq res :parallel)
	   (list (line-a l2)))
	  ((on-segment-p l2 res)
	   (list res)))))

(defmethod intersection-object ((l2 line) (l1 long-line))
  (intersection-object l1 l2))

(defmethod intersection-object ((l1 line) (l2 line))
  (let ((res (intersection-line l1 l2)))
    (cond ((null res) nil)
	  ((eq res :parallel)
	   ;; XXX flubbidel
	   nil)
	  ((and (on-segment-p l1 res)
		(on-segment-p l2 res))
	   (list res)))))

(defmethod intersection-object ((a1 arc) (l2 long-line))
  (let ((results (intersection-line-circle (long-line-a l2) (long-line-b l2) (arc-centre a1)
				       (line-length (make-line :a (arc-a a1)
							       :b (arc-centre a1))))))
    (loop for res in results
	 when (on-arc-p a1 res)
	 collect res)))

(defmethod intersection-object ((l2 long-line) (a1 arc))
  (intersection-object a1 l2))

(defmethod intersection-object ((a1 arc) (l2 line))
  (let ((results (intersection-line-circle (line-a l2) (line-b l2) (arc-centre a1)
				       (line-length (make-line :a (arc-a a1)
							       :b (arc-centre a1))))))
    (loop for res in results
	 when (and (on-arc-p a1 res)
		   (on-segment-p l2 res))
	 collect res)))

(defun intersection-2-circles (c1 c2)
  (let ((d (line-length (make-line :a (circle-centre c1)
				   :b (circle-centre c2))))
	(r1 (circle-radius c1))
	(r2 (circle-radius c2))
	(x1 (2d-point-x (circle-centre c1)))
	(y1 (2d-point-y (circle-centre c1)))
	(x2 (2d-point-x (circle-centre c2)))
	(y2 (2d-point-y (circle-centre c2))))

    (cond ((> d (+ r1 r2))
	   nil)
	  ((< d (abs (- r1 r2)))
	   nil)
	  ((and (= d 0)
		(= r1 r2))
	   t)
	  (t (let* ((dis (!! (sqrt (((square (r1 + r2)) - (square d)) *
				     ((square d) - (square (r2 - r1)))))))
		    (coeff (!! ((square r1) - (square r2)) / (2 * (square d))))
		    (x_first (!! ((x1 + x2) / 2) + coeff * (x2 - x1)))
		    (y_first (!! ((y1 + y2) / 2) + coeff * (y2 - y1)))
		    (_x1 (!! x_first + ((y2 - y1) / (2 * (square d))) * dis))
		    (_y1 (!! y_first - ((x2 - x1) / (2 * (square d))) * dis))
		    (_x2 (!! x_first - ((y2 - y1) / (2 * (square d))) * dis))
		    (_y2 (!! y_first + ((x2 - x1) / (2 * (square d))) * dis)))
;;	       (format t "dis: ~A~%" dis)
	       (if (or (= d (+ r2 r1))
		       (and (= _x1 _x2)
			    (= _y1 _y2)))
		   (list (2dp _x1 _y1))
		   (list (2dp _x1 _y1)
			 (2dp _x2 _y2))))))))

(deftest :intersection "2 circles"
  (let ((c1 (make-circle :centre (2dp 0 0) :radius 2))
	(c2 (make-circle :centre (2dp 4 0) :radius 2)))
    (format t "interesection 2 circles: ~A~%" (intersection-2-circles c1 c2))))

(defmethod intersection-object ((l1 line) (a2 arc))
  (intersection-object a2 l1))

(defmethod intersection-object ((c1 circle) (c2 circle))
  (intersection-2-circles c1 c2))

(defmethod intersection-object ((a1 arc) (a2 arc))
  (let ((points (intersection-object (make-circle :centre (arc-centre a1)
						  :radius (arc-radius a1))
				     (make-circle :centre (arc-centre a2)
						  :radius (arc-radius a2)))))
    (cond ((eq points t)
	   (if (arc-= a1 a2)
	       (progn
		 (format t "equal arcs ~A ~A~%" a1 a2)
		 (error "equal arcs ~A ~A" a1 a2))
	       (let ((res))
		 (when (point-= (arc-a a1) (arc-b a2))
		   (push (arc-a a1) res))
		 (when (point-= (arc-a a1) (arc-a a2))
		   (push (arc-a a1) res))
		 (when (point-= (arc-b a1) (arc-b a2))
		   (push (arc-b a1) res))
		 (when (point-= (arc-b a1) (arc-a a2))
		   (push (arc-b a1) res))
		 res)))
	  (t
	   (remove-if-not #'(lambda (x)
			      (and (on-arc-p a1 x)
				   (on-arc-p a2 x))) points)))))

(deftest :intersection "intersection 2 segments"
  (test-assert (point-= (first (intersection-object
				(make-line :a (2dp 0 0) :b (2dp 0 1))
				(make-line :a (2dp 0 1) :b (2dp 1 1))))
			(2dp 0 1)))

  (test-assert (point-= (first (intersection-object
				(make-line
				   :A (make-2D-POINT :X 200.0 :Y 35.0)
				   :B (make-2D-POINT :X 30.0 :Y 35.0))
				(make-line
				   :A (make-2D-POINT :X 35.0 :Y 30.0)
				   :B (make-2D-POINT :X 35.0 :Y 200.0))))
			(2dp 35 35))))

  (test-assert (null (intersection-object
		      (make-line :a (2dp 0 0) :b (2dp 0 1))
		      (make-line :a (2dp 0 1.1) :b (2dp 1 1)))))
  

(defgeneric offset-object (object offset))

(defmethod offset-object ((l1 line) offset)
  (offset-line l1 offset))

(defmethod offset-object ((a1 arc) offset)
  (offset-arc a1 offset))

(defun segment-end (seg)
  (cond ((typep seg 'line)
	 (line-b seg))
	((typep seg 'arc)
	 (arc-b seg))
	((typep seg 'list)
	 (segment-end (first (last seg))))))

(defun circular-segment-p (seg)
  (point-epsilon= (segment-start seg) (segment-end seg)))

(defun segment-start (seg)
  (cond ((typep seg 'line)
	 (line-a seg))
	((typep seg 'arc)
	 (arc-a seg))
	((typep seg 'list)
	 (segment-start (first seg)))))

(defun seg-stop-at (seg at)
  (cond ((typep seg 'line)
	 (make-line :a (line-a seg)
		    :b at))
	((typep seg 'arc)
	 (make-arc :a (arc-a seg)
		   :b at
		   :centre (arc-centre seg)
		   :direction (arc-direction seg)))))

(defun seg-start-at (seg at)
  (cond ((typep seg 'line)
	 (make-line :a at
		    :b (line-b seg)))
	((typep seg 'arc)
	 (make-arc :b (arc-b seg)
		   :a at
		   :centre (arc-centre seg)
		   :direction (arc-direction seg)))))

(defun object-line (object)
  (cond ((typep object 'line)
	 object)
	((typep object 'arc)
	 (make-line :a (arc-a object) :b (arc-b object)))))
	 

(defun offset-curve (curve offset &key (step nil))
  (let* ((curve2
	  curve
	   )
	 (steps 0)
	 (last-part (first curve2))
	 (last-seg (offset-object (first curve2) offset))
	 (last-last-seg (offset-object (car (last curve2)) offset))
	 (intersect (intersection-object last-seg last-last-seg))

	 res)

;;    (setf curve2 (wrap-around curve2))
    (when intersect
      (setf last-seg (seg-start-at last-seg (first intersect))))

    (flet ((insert-seg (seg)
;;	     (format t "INSERT SEG: ~A~%" seg)
	     (setf res (insert-segment-with-intersection res seg))

	     (unless (null step)
	       (sdl:with-color (col (sdl:color :r 0 :g 0 :b 0))
		 (sdl:clear-display col))
	       
	       (sdl:with-color (col (sdl:color :r 255 :g 0 :b 0))
		 (draw-curve curve))
	       
	       (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
		 (draw-curve res))
	       (sdl:update-display))

	     #+nil(push seg res)))
      
    (loop for part in (cdr curve2)
       for seg = (offset-object part offset)
       for intersect = (when seg (intersection-object last-seg seg))

       do

	 (unless (null step)
	   (sdl:with-color (sdl:*default-color* (sdl:color :r 0 :g 0 :b 255))
	     (draw-curve (list seg))
	     (sdl:with-color (sdl:*default-color* (sdl:color :r 0 :g 200 :b 255))
	       (draw-curve (list last-seg)))
	     (sdl:update-display))
	   (when (= steps 0)
	     (setf steps (read)))
	   
	   (decf steps))

	 (cond ((null intersect)
		(insert-seg last-seg)
		(when (null intersect)
		  (let* ((length (line-length (make-line :a (segment-start seg)
							 :b (segment-end last-seg))))
			 
			 (arc (if (< length 0.1)
				  (make-line :a (segment-end last-seg)
					     :b (segment-start seg))
				  (make-arc :a (segment-end last-seg)
					    :b (segment-start seg)
					    :centre (segment-start part)
					    :direction (if (> offset 0)
							   :cw
							   :ccw)))))
		    ;;		    (format t "insert ar intersect: ~A ~A~%" arc length)
		    (insert-seg arc))))
	       (t
		(insert-seg (seg-stop-at last-seg (first intersect)))))
	 
	 (setf last-seg seg)
	 
	 (setf last-part part)))
    (push (make-line :a (object-end-point (first res))
		     :b (object-start-point (first (last res)))) res)
    (nreverse res)))

(defun insert-segment-with-intersection (segments segment)
  (let ((restliste segments)
	(restintersection))
    (do* ((list segments (cdr list))
	  (elt (first segments) (first list)))
	 
	 ((null list)
	  (if (null restintersection)
	      (cons segment segments)

	      (append (list (seg-start-at segment restintersection)
			    (seg-stop-at (first restliste) restintersection))
		      (cdr restliste))))
      
      (let ((intersection (intersection-object elt segment)))
	(cond ((= (length intersection) 2)
	       (warn "arcs schneiden sich in 2 punkt, igitt, nicht jetzt"))
	      ((= (length intersection) 1)
	       (let ((intersection (first intersection)))
;;		 (format t "objekte shcneiden sich in ~A~%" intersection)
		 (setf restintersection intersection
		       restliste list))))))))

(deftest :insert-segment-with-intersection ()
  (let ((l1 (list
	     (make-line :a (2dp 0 10) :b (2dp 0 20))
	     (make-line :a (2dp 0 0) :b (2dp 0 10)))))
    (insert-segment-with-intersection l1 (make-line :a (2dp -1 4) :b (2dp 10 4)))))

(defparameter *test-curve*
  (list (make-line :a (2dp 30 30) :b (2dp 200 30))
	(make-line :a (2dp 200 30) :b (2dp 200 200))
	(make-line :a (2dp 200 200) :b (2dp 30 200))
	(make-line :a (2dp 30 200) :b (2dp 30 30))))

(defun make-line-curve (coords)
  (let (res)
    (loop for (_a _b) on coords
       when (null _b)
       do (setf _b (first coords))
       do
	 (let ((a (2dp (first _a) (second _a)))
	       (b (2dp (first _b) (second _b))))
	   (push (make-line :a a :b b) res)))
    (nreverse res)))
				 
(defparameter *test-curve2*
  (make-line-curve '((30 30) (30 200) (200 200) (200 30))))

(defparameter *test-curve3*
  (make-line-curve (reverse '((150 110) (140 120) (150 130) (120 130)
			      (150 140) (160 120) (150 130)))))


(defparameter *test-curve4*
  (list (make-arc :a (2dp 100 90) :b (2dp 90 100) :centre (2dp 100 100) :direction :cw)
	(make-arc :a (2dp 90 100) :b (2dp 100 110) :centre (2dp 100 100) :direction :cw)
	(make-arc :a (2dp 100 110) :b (2dp 110 100) :centre (2dp 100 100) :direction :cw)
	(make-arc :a (2dp 110 100) :b (2dp 100 90) :centre (2dp 100 100) :direction :cw)))

(defparameter *test-curve5*
  (list (make-arc :a (2dp 100 90) :b (2dp 90 100) :centre (2dp 100 100) :direction :cw)
	(make-arc :a (2dp 90 100) :b (2dp 80 110) :centre (2dp 80 100) :direction :ccw)
	(make-arc :a (2dp 80 110) :b (2dp 90 120) :centre (2dp 90 110) :direction :cw)
	(make-arc :a (2dp 90 120) :b (2dp 100 110) :centre (2dp 90 110) :direction :cw)
	(make-arc :a (2dp 100 110) :b (2dp 110 100) :centre (2dp 100 100) :direction :cw)
	(make-arc :a (2dp 110 100) :b (2dp 100 90) :centre (2dp 100 100) :direction :cw)))

(defparameter *test-curve6*
  (list (make-arc :a (2dp 90 100) :b (2dp 100 110) :centre (2dp 100 100) :direction :cw)
	(make-arc :a (2dp 100 110) :b (2dp 110 120) :centre (2dp 110 110) :direction :cw)
	(make-line :a (2dp 110 120) :b (2dp 110 100))
	(make-line :a (2dp 110 100) :b (2dp 90 100))))

(defparameter *test-arc*
  (list (make-arc :a (2dp 100 90) :b (2dp 90 100) :centre (2dp 100 100))
	(make-arc :a (2dp 110 100) :b (2dp 100 90) :centre (2dp 100 100))))

(defun reverse-curve (curve)
  (let ((res))
    (loop for elt in curve
	 do (cond ((typep elt 'arc)
		   (with-slots (a b centre direction) elt
		     (push
		      (make-arc :a b :b a :centre centre :direction (case direction
								      (:cw :ccw)
								      (:ccw :cw)))
		      res)))
		  ((typep elt 'line)
		   (with-slots (a b) elt
		     (push (make-line :a b :b a) res)))))
    res))
       

(defparameter *test-curve7*
  (reverse-curve
   (list (make-line :a (2dp 120 100) :b (2dp 100 100))
	 (make-arc :a (2dp 100 100) :b (2dp 100 104) :centre (2dp 100 102) :direction :cw)
	 (make-arc :a (2dp 100 104) :b (2dp 100 108) :centre (2dp 100 106) :direction :cw)
	 (make-arc :a (2dp 100 108) :b (2dp 100 112) :centre (2dp 100 110) :direction :cw)
	 (make-arc :a (2dp 100 112) :b (2dp 100 116) :centre (2dp 100 114) :direction :cw)
	 (make-arc :a (2dp 100 116) :b (2dp 100 120) :centre (2dp 100 118) :direction :cw)
	 (make-line :a (2dp 100 120) :b (2dp 120 120))
	 (make-line :a (2dp 120 120) :b (2dp 120 100)))))


(defun sdl-curve-offset (curve offset)
  (let ((curve (curve-to-arcs curve)))
    (sdl:with-init ()
      (sdl:window 1200 480)
      (setf (sdl:frame-rate) -1)
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
      
      (sdl:with-color (col (sdl:color :r 255 :g 0 :b 0))
	(draw-curve curve :color col))
      (cond ((listp offset)
	     (dolist (off offset)
	       (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
		 (draw-curve (offset-curve curve off) :color col))))
	    (t (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
		 (draw-curve (offset-curve curve offset) :color col))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)))))

(defun sdl-curve-step (curve offset)
  (let ((curve (curve-to-arcs curve)))
    (sdl:with-init ()
      (sdl:window 1240 480)
      (setf (sdl:frame-rate) -1)
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))

      (offset-curve curve offset :step t)
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)))))


(defparameter *test-curve8*
  (list (make-ARC
       :A (make-2D-POINT :X 122.70350720208663d0 :Y 91.4762717112958d0)
       :B (make-2D-POINT :X 129.17455000574907d0 :Y 80.83247047936504d0)
       :CENTRE (make-2D-POINT :X 135.0327466532994d0 :Y 91.68301987970003d0)
       :DIRECTION :CCW)
    (make-ARC
       :A (make-2D-POINT :X 129.17455000574907d0 :Y 80.83247047936504d0)
       :B (make-2D-POINT :X 143.4106116342254d0 :Y 80.60407206612663d0)
       :CENTRE (make-2D-POINT :X 136.5449449127566d0 :Y 96.44811250298646d0)
	      :DIRECTION :CCW)
    (make-ARC
       :A (make-2D-POINT :X 143.4106116342254d0 :Y 80.60407206612663d0)
       :B (make-2D-POINT :X 144.77168014187748d0 :Y 80.73988759002842d0)
       :CENTRE (make-2D-POINT :X 144.1751993954996d0 :Y 79.82964178998908d0)
       :DIRECTION :CW)
    (make-ARC
       :A (make-2D-POINT :X 144.77168014187748d0 :Y 80.73988759002842d0)
       :B (make-2D-POINT :X 144.52164220608933d0 :Y 78.90127172899423d0)
       :CENTRE (make-2D-POINT :X 142.6560272385542d0 :Y 80.09129091066121d0)
       :DIRECTION :CW)
    (make-ARC
       :A (make-2D-POINT :X 144.52164220608933d0 :Y 78.90127172899423d0)
       :B (make-2D-POINT :X 130.17592040803706d0 :Y 58.75405954637904d0)
       :CENTRE (make-2D-POINT :X 96.12895779148438d0 :Y 98.17803507637039d0)
       :DIRECTION :CW)
    (make-LINE
       :A (make-2D-POINT :X 130.17592040803706d0 :Y 58.75405954637904d0)
       :B (make-2D-POINT :X 123.67971459857519d0 :Y 52.00811909275808d0))))

(defun bla ()
  (with-transform ((translation-matrix -100 -100))
    (with-transform ((scaling-matrix 4))
      (sdl-curve-step *test-curve7* 5))))

(deftest :intersection "line interesection"
  (test-assert (point-=
		(first (intersection-object (make-line :a (2dp 115 115)
						       :b (2dp 100 115))
					    (make-line :a (2dp 105 116)
						       :b (2dp 105 112))))
		(2dp 105 115))))