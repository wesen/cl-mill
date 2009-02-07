(in-package :gcode)

(defun face-top ()
  (g-program
    (with-tool (*test-tool*)
      (spindle-on)
      (mill-abs :z 2)
      (load-file "/Users/manuel/dxf/output-1.lisp"))))

(defun face-file (file)
  (g-program
    (with-tool (*test-tool*)
      (spindle-on)
      (mill-abs :z 2)
      (load-file file))))
