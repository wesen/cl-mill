(in-package :gcode)

(defparameter *alu-tool*
  (make-instance 'tool  :diameter 2 :depth 0.3 :number 11))

(defparameter *platte-depth* 3.2)

(defun klemme ()
  (let ((tool *alu-tool*)
        (depth *platte-depth*))
    (with-named-pass ("drills")
      (with-tool (tool)
        (drill :x 10 :y 10 :diameter 5 :depth depth)))
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline 20 20 :depth depth)))))

(defun minicommand-halterung ()
  (let ((tool *alu-tool*)
        (depth *platte-depth*))
    (with-program ("halterung")
      (with-named-pass ("drills")
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (goto-abs :z *fly-height*)))

      (with-named-pass ("mill")
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (goto-abs :z *fly-height*)))

      #|
      (with-named-pass ("drills")
        (with-tool (tool)
          ;; platte halterung
          (drill :x 10 :y 10 :diameter 5 :depth depth)
          (drill :x 10 :y 130 :diameter 5 :depth depth)
          (drill :x 170 :y 10 :diameter 5 :depth depth)
          (drill :x 170 :y 130 :diameter 5 :depth depth)
          
          ;; minicommand
          (drill :x 35.25 :y 27.9 :diameter 4 :depth depth)
          (drill :x 35.25 :y (+ 27.9 84.2) :diameter 4 :depth depth)
          (drill :x (+ 35.25 109.5) :y 27.9 :diameter 4 :depth depth)
          (drill :x (+ 35.25 109.5) :y (+ 27.9 84.2) :diameter 4 :depth depth)

          ))

      (with-named-pass ("mill")
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (rectangle-outline 180 140 :depth depth)))

      (with-named-pass ("umrandung")
        (goto-abs :x 30.5 :y 23.25)
        (rectangle 119 93.5))
      |#

      ;; klemmen
      (dotimes (i 4)
        (with-transform ((translation-matrix 190 (* i 30)))
          (klemme)))

      )))

          