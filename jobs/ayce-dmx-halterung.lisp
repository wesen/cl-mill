(in-package :gcode)

(defparameter *alu-tool-ayce-dmx-halterung*
  (make-instance 'tool :diameter 2 :depth 0.2 :number 11 :feed-xy 500 :feed-z 100))

(defun test-loch (depth)
  (with-named-pass ("drills")
    (drill :x 5 :y 5 :diameter 4 :depth depth)
    #+nil(drill :x 10 :y 5 :diameter 4 :depth depth)))

(defun innere-halterung (depth)
  (with-named-pass ("halterung")
    (goto-abs :x 0 :y 0)
    (rectangle-inline 3.5 11.3 :depth depth)
    (goto-abs :x 7 :y 0)
    (rectangle-inline 3.5 11.3 :depth depth)
    (goto-abs :x 14 :y 0)
    (rectangle-inline 21.2 11.3 :depth depth)
    (goto-abs :x (+ 14 21.2 3.5) :y 0)
    (rectangle-inline 3.5 11.3 :depth depth)
    (goto-abs :x (+ 14 21.2 10.5) :y 0)
    (rectangle-inline 3.5 11.3 :depth depth)))

(defun ayce-dmx-halterung ()
  (let ((tool *alu-tool-ayce-dmx-halterung*)
        (*grundplate-depth* 4))

    (with-program ("grundplatte")
      (with-named-pass ("drills")
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (goto-abs :z *fly-height*)))

      (with-named-pass ("drills")
        (with-tool (tool)
          (drill :x 4.5 :y 5 :diameter 4 :depth *grundplate-depth*)
          (drill :x 4.5 :y 45 :diameter 4 :depth *grundplate-depth*)
          (drill :x 209.5 :y 5 :diameter 4 :depth *grundplate-depth*)
          (drill :x 209.5 :y 45 :diameter 4 :depth *grundplate-depth*)))

      (with-named-pass ("umrandung")
        (with-tool (tool)
          (goto-abs :x 0 :y 0)
          (rectangle-outline 214 50 :depth *grundplate-depth*)))
      
      (with-named-pass ("halterung")
        (with-tool (tool)

          #+nil(with-transform ((translation-matrix 19 38))
            (with-transform ((rotation-matrix 45))
              (innere-halterung *grundplate-depth*)))

          #+nil(with-transform ((translation-matrix 187 46))
            (with-transform ((rotation-matrix 135))
              (innere-halterung *grundplate-depth*)))
            )))
    
            
      
))