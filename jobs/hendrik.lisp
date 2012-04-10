(in-package :gcode)

(defparameter *metal-tool*
  (make-instance 'tool :diameter 2 :depth 0.1 :number 8 :feed-xy 500 :feed-z 100))

(defun mill-line (&key x y (depth (tool-depth *current-tool*)))
  (let ((nums (/ depth (tool-depth *current-tool*))))
    (if (> nums 1)
        (loop for i from 0 below nums by 2
           with orig-x = (orig-current-x)
           with orig-y = (orig-current-y)
           for curdepth from (tool-depth *current-tool*) by (* 2 (tool-depth *current-tool*))
           do (tool-down :depth curdepth)
             (mill-abs :x x :y y)
             (tool-down :depth (+ curdepth (tool-depth *current-tool*)))
             (mill-abs :x orig-x :y orig-y)
           finally (tool-up))
        (with-tool-down (depth)
          (mill-abs :x x :y y)))))

(defun hendrik-job ()
  (with-program ("hendrik")
    (with-named-pass ("mill")
      (with-tool (*metal-tool*)
        (goto-abs :x 0 :y 0)
        (goto-abs :z *fly-height*)
        (mill-line :x 0 :y 46 :depth 3)))))
