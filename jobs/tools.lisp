(in-package :Gcode)

(defparameter *alu-tool-3*
  (make-instance 'tool :diameter 3 :depth 0.2 :number 11))

(defparameter *platte-depth* 10)

(defun align-block (&key (tool *alu-tool-3*) (depth *platte-depth*) (height 20))
  (format t "tool: ~A~%" (tool-diameter tool))
  (with-program ("align-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))
      
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline 60 height :depth depth)))

    (with-named-pass ("drills")
      (with-tool (tool)
        (drill :x 10 :y 10 :diameter 5 :depth depth)
        (drill :x 50 :y 10 :diameter 5 :depth depth)))))

(defun align-block-long (&key (tool *alu-tool-3*) (depth *platte-depth*))
  (with-program ("align-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))
      
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline 180 20 :depth depth)))

    (with-named-pass ("drills")
      (with-tool (tool)
        (dotimes (i 5)
          (drill :x (+ 10 (* i 40)) :y 10 :diameter 5 :depth depth))))))

(defun hold-plate (&key (tool *alu-tool-3*) (depth *platte-depth*))
  (with-program ("align-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))
      
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline 60 20 :depth depth)))

    (with-named-pass ("drills")
      (with-tool (tool)
        (dotimes (i 5)
          (drill :x (+ 10 (* i 10)) :y 10 :diameter 5 :depth depth))))))

(defun hold-plate-2 (&key (tool *alu-tool-3*) (depth *platte-depth*) (width 80))
  (with-program ("align-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))
      
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 10 :y 7.5)
        (rectangle-inline (- width 20) 5 :depth depth)
        (goto-abs :x 0 :y 0)
        (rectangle-outline width 20 :depth depth)))))


(defun hold-block (&key (tool *alu-tool-3*) (depth *platte-depth*) (width 30) (height 30))
  (with-program ("align-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))
      
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline width height :depth depth)))))

      
(defun minicommand-base-block (&key (tool *alu-tool-3*) (depth *platte-depth*))
  (with-program ("minicommand-base-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-transform ((translation-matrix 10 20))
      (with-named-pass ("mill")
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (rectangle-inline 119 93.5 :depth 2)
          (goto-abs :x 0.7 :y 0.7)
          (rectangle-inline (- 119 1.4) (- 93.5 1.4) :depth 2)))

      (with-named-pass ("drills")
        (with-tool (tool)
          (drill :x 5 :y 4.65 :diameter 3.5 :depth depth)
          (drill :x 5 :y (- 93.5 4.65) :diameter 3.5 :depth depth)
          (drill :x (- 119 5) :y 4.65 :diameter 3.5 :depth depth)
          (drill :x (- 119 5) :y (- 93.5 4.65) :diameter 3.5 :depth depth))))
    
    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline 140 140 :depth depth)))

    (with-named-pass ("drills")
      (with-tool (tool)
        (drill :x 10 :y 5 :diameter 5 :depth depth)
        (drill :x 130 :y 5 :diameter 5 :depth depth)
        (drill :x 10 :y 130 :diameter 5 :depth depth)
        (drill :x 130 :y 130 :diameter 5 :depth depth)))))

(defun minicommand-base-block-2 (&key (tool *alu-tool-3*) (depth *platte-depth*))
  (with-program ("minicommand-base-block")
    (with-named-pass ("drills")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)))

    (with-named-pass ("mill")
      (with-tool (tool)
        (goto-abs :x 0 :y 0)
        (rectangle-outline 140 140 :depth depth)
        (goto-abs :x 5 :y 5)
        (rectangle-inline 10 10 :depth depth)
        (goto-abs :x 5 :y 125)
        (rectangle-inline 10 10 :depth depth)))

    (with-named-pass ("drills")
      (with-tool (tool)
        (dotimes (x 3)
          (dotimes (y 4)
            (drill :x (+ 50 (* x 40)) :y (+ 10 (* y 40)) :depth depth :diameter 5)))))))
  
  

  