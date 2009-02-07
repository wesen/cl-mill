(in-package :gcode)

(defstruct arc
  a b centre direction)

(defun arc-radius (arc)
  (line-length (make-line :a (arc-a arc) :b (arc-centre arc))))

(defun arc-angle (arc)
  (with-slots (a b centre direction) arc
    (let ((angle (case direction
		   (:ccw (angle-2-segments-directed (make-line :a centre :b a)
						   (make-line :a centre :b b)))
		   (:cw (angle-2-segments-directed (make-line :a centre :b b)
						   (make-line :a centre :b a))))))
      (if (< angle 0)
	  (+ angle (* 2 *PI*))
	  angle))))

(deftest :arc "Test arc angle cw"
  (let ((arc (make-arc :centre (2dp 0 0)
		       :a (2dp -1 0)
		       :b (2dp 0 1)
		       :direction :cw)))
    (test-assert (epsilon-= (arc-angle arc) (* *PI* 1/2)))))

(deftest :arc "Test arc angle ccw"
  (let ((arc (make-arc :centre (2dp 0 0)
		       :a (2dp -1 0)
		       :b (2dp 0 1)
		       :direction :ccw)))
    (test-assert (epsilon-= (arc-angle arc) (* *PI* 3/2)))))

(defun test-angle-2-segments-directed ()
  (let ((l1 (make-line :a (2dp 0 0) :b (2dp 1 0)))
	(l2 (make-line :a (2dp 0 0) :b (2dp 0 1)))
	(l3 (make-line :a (2dp 0 0) :b (2dp -1 0))))
    (format t "angle ~A ~A :~A~%" l1 l2 (angle-2-segments-directed l1 l2))
    (format t "angle ~A ~A :~A~%" l2 l1 (angle-2-segments-directed l2 l1))
    (format t "angle ~A ~A :~A~%" l1 l2 (angle-2-segments-directed l1 l3))
    (format t "angle ~A ~A :~A~%" l2 l1 (angle-2-segments-directed l3 l1))))

;;; ON-ARC-P

(defun on-arc-p (arc p)
  (and (epsilon-= (arc-radius arc) (line-length (make-line :a p :b (arc-centre arc))))
       (with-slots (a b direction centre) arc
	 (let ((a1 (arc-angle arc))
	       (a2 (arc-angle (make-arc :a a :b p :centre centre
			      :direction direction))))
	 (<= a2 a1)))))

(deftest :arc "On arc tests"
  (let ((arc1 (make-arc :centre (2dp 0 0)
			:a (2dp -1 0)
			:b (2dp 0 1)
			:direction :cw))
	(arc2 (make-arc :centre (2dp 0 0)
			:a (2dp 0 1)
			:b (2dp -1 0)
			:direction :ccw))
	(arc3 (make-arc :centre (2dp 0 0)
			:a (2dp 0 1)
			:b (2dp -1 0)
			:direction :cw))
	(p1 (2dp -1 0))
	(p2 (2dp 0 0))
	(p3 (2dp 0 1))
	(p4 (2dp 0 -1))
	(p5 (2dp (cos (/ *PI* 4))
		 (sin (/ *PI* 4))))
	(p6 (2dp (cos (* *PI* 3/4))
		 (sin (* *PI* 3/4)))))
    (test-assert (on-arc-p arc1 p1))
    (test-assert (not (on-arc-p arc1 p2)))
    (test-assert (on-arc-p arc1 p3))
    (test-assert (not (on-arc-p arc1 p4)))
    (test-assert (not (on-arc-p arc1 p5)))
    (test-assert (on-arc-p arc1 p6))

    (test-assert (on-arc-p arc2 p1))
    (test-assert (not (on-arc-p arc2 p2)))
    (test-assert (on-arc-p arc2 p3))
    (test-assert (not (on-arc-p arc2 p4)))
    (test-assert (not (on-arc-p arc2 p5)))
    (test-assert (on-arc-p arc2 p6))

    (test-assert (on-arc-p arc3 p1))
    (test-assert (not (on-arc-p arc3 p2)))
    (test-assert (on-arc-p arc3 p3))
    (test-assert (on-arc-p arc3 p4))
    (test-assert (on-arc-p arc3 p5))
    (test-assert (not (on-arc-p arc3 p6)))
    ))
    
(defmethod object-start-point ((arc arc))
  (arc-a arc))

(defmethod object-end-point ((arc arc))
  (arc-b arc))