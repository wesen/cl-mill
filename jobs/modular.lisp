(in-package :gcode)

(defparameter *holz-tool-modular*
  (make-instance 'tool :diameter 1 :depth 0.2 :number 3))

(defparameter *alu-tool-modular*
  (make-instance 'tool :diameter 2 :depth 0.2 :number 11))


(defparameter *frontplate-depth* 3)

(defun alpha-pot (&optional (depth *frontplate-depth*))
  "alpha pot halterung, rectangle: 20 x 30"
  (goto-abs :x 0 :y 0)
  (drill :x 10 :y 10 :diameter 7 :depth depth)
  (drill :x (- 10 2.54) :y 26 :diameter 1 :depth depth)
  (drill :x 10 :y 26 :diameter 1 :depth depth)
  (drill :x (+ 10 2.54) :y 26 :diameter 1 :depth depth))

(defun jack (&optional (depth *frontplate-depth*))
  "jack halterung, rectangle 15 x 20"
  (drill :x 7.5 :y 6 :diameter 8 :depth depth)
  (drill :x (- 7.5 2.54) :y 16 :diameter 1 :depth depth)
  (drill :x 7.5 :y 16 :diameter 1 :depth depth)
  (drill :x (+ 7.5 2.54) :y 16 :diameter 1 :depth depth))

(defun modular-test-panel (panel-width pot-rows jack-rows &key (tool *alu-tool-modular*))
  (let (
        (panel-height (+ 10 40 (* pot-rows 30) (* jack-rows 20))))

    (with-program ("modular-test")
      (with-named-pass ("drills")
        
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (goto-abs :z *fly-height*)))
      
      
      (with-transform ((translation-matrix 0 panel-width))
        (with-transform ((rotation-matrix 90))
          (with-named-pass ("umrandung")
            (with-tool (tool)
              ;; umrandung der frontplatte
              (goto-abs :x 0 :y 0)
              (rectangle-outline panel-width panel-height :depth *frontplate-depth*)))

          (with-named-pass ("drills")
            ;; befestigungen fuer die frontplatte
            (with-tool (tool)
              (let ((y (- panel-height 20)))
                
              (drill :x 25 :y y :diameter 3 :depth *frontplate-depth*)
              (drill :x (- panel-width 25) :y y :diameter 3 :depth *frontplate-depth*))))
            

          (with-transform ((translation-matrix 10 10))
            (with-named-pass ("drills")
              (with-tool (tool)
                ;; alpha - pots
                (dotimes (x (floor (/ (- panel-width 20) 20)))
                  (dotimes (y pot-rows)
                    (format t "pot at ~A, ~A~%" x y)
                    (with-transform ((translation-matrix (* x 20) (* y 30)))
                      (alpha-pot))))

                (with-transform ((translation-matrix 2.5 (* pot-rows 30)))
                  (dotimes (x (floor (/ (- panel-width 20) 15)))
                    (dotimes (y jack-rows)
                      (format t "jack at ~A, ~A~%" x y)
                      (with-transform ((translation-matrix (* x 15) (* y 20)))
                        (jack)))))))))))))


        