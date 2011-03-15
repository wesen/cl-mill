(in-package :gcode)


(defparameter *alu-tool-ayce*
  (make-instance 'tool :diameter 2 :depth 0.4 :number 11 :feed-xy 500 :feed-z 100))

(defparameter *alu-tool-ayce-fast*
  (make-instance 'tool :diameter 2 :depth 0.4 :number 8 :feed-xy 500 :feed-z 100))


(defun rj45-platte (depth)
  (with-named-pass ("drills")
    (drill :x 3.81 :y 30.48 :diameter 3 :depth depth)
    (drill :x 82.55 :y 30.48 :diameter 3 :depth depth)
    (drill :x 166.37 :y 30.48 :diameter 3 :depth depth)
    (drill :x 3.81 :y 68.58 :diameter 3 :depth depth)
    (drill :x 82.55 :y 55.88 :diameter 3 :depth depth)
    (drill :x 166.37 :y 68.58 :diameter 3 :depth depth))
  (with-named-pass ("umrandung")
    (goto-abs :x 0 :y 0)
    (rectangle 170.16 72.06)))

(defun atmel-platte (depth)
  (with-named-pass ("drills")
    (drill :x 5.08 :y 3.81 :diameter 3 :depth depth)
    (drill :x 5.08 :y 68.58 :diameter 3 :depth depth)
    (drill :x 97.79 :y 68.58 :diameter 3 :depth depth)
    (drill :x 97.79 :y 5.08 :diameter 3 :depth depth))
  (with-named-pass ("umrandung")
    (goto-abs :x 0 :y 0)
    (rectangle 101.58 72.06)))

(defun connector-platte (depth)
  (with-named-pass ("drills")
    (drill :x 3.81 :y 2.54 :diameter 3 :depth depth)
    (drill :x 3.81 :y 81.28 :diameter 3 :depth depth)
    (drill :x 71.12 :y 81.28 :diameter 3 :depth depth)
    (drill :x 71.12 :y 2.54 :diameter 3 :depth depth))

  (with-named-pass ("umrandung")
    (goto-abs :x 0 :y 0)
    (rectangle 74.91 84.76)))

(defun eddy-platte (depth)
  (with-named-pass ("drills")
    (drill :x 3.36 :y 3.36 :diameter 3 :depth depth)
    (drill :x 3.36 :y 66.64 :diameter 3 :depth depth)
    (drill :x 101.64 :y 66.64 :diameter 3 :depth depth)
    (drill :x 101.64 :y 3.36 :diameter 3 :depth depth)
    )
  (with-named-pass ("umrandung")
    (goto-abs :x 0 :y 0)
    (rectangle 105 70)
    ))

(defun ayce-grund-platte ()
  (let ((tool *alu-tool-ayce*)
	(*grundplate-depth* 2))

    (with-program ("grundplatte")
      (with-named-pass ("drills")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z *fly-height*)))

      (with-tool (tool)
	(with-transform ((translation-matrix 15 0))
	  
	  ;; first dmx board
	  #+nil
	  (rj45-platte *grundplate-depth*)

	  (with-named-pass ("drills")
	    (goto-abs :x 0 :y 20))
	  
	  ;; second dmx board
	  (with-transform ((translation-matrix 172 0))
	  (rj45-platte *grundplate-depth*))
	  
	  ;; atmel board
	  (with-transform ((translation-matrix 4 122))
	  (atmel-platte *grundplate-depth*))
	  
;;	  (with-named-pass ("svg")
;;	    (svg-pass "/Users/manuel/Pictures/headcraft.svg" :depth *grundplate-depth*))
	  
	  ;; connector board
	  (with-transform ((translation-matrix 115 122))
	  (connector-platte *grundplate-depth*))
	  
	  ;; eddy board
	  (with-transform ((translation-matrix 218 133))
	  (eddy-platte *grundplate-depth*))
	  )))))
  
(defun rj45-connector (x y depth)
  ;; 16.5 * 12.8e
  (let ((w 17)
	(h 13.3))
    (with-named-pass ("drills")
      (goto-abs :x (- x (/ w 2))
		:y (- y (/ h 2)))
      (rectangle-inline w h :depth depth)))
  #+nil
  (let ((w 16.5)
	(h 12.8))
    (with-named-pass ("drills")
      (goto-abs :x (- x (/ w 2))
		:y (- y (/ h 2)))
      (rectangle-inline w h :depth depth))))
  

(defun rj45-connectors (depth)
  #|
  (rj45-connector 12.7 0 depth)
  (rj45-connector 33.02 0 depth)
  (rj45-connector 53.34 0 depth)
  (rj45-connector 73.66 0 depth)
  (rj45-connector 93.98 0 depth)
  (rj45-connector 114.3 0 depth)
  (rj45-connector 135.89 0 depth)
  |#
  (rj45-connector 157.48 0 depth))

(defun ayce-front-platte ()
  (let ((tool *alu-tool-ayce-fast*)
	(*frontplate-depth* 4.1))

    (with-program ("frontplatte")
      (with-named-pass ("drills")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z 30)
	  (goto-abs :x 35 :y 22)
	  (goto-abs :z *fly-height*)

	  ;; 30mm til center of screw + 1.5mm till start of gorund plate
	  ;; 8 mm height ?!?
	  #+nil
	  (with-transform ((translation-matrix (+ (- 30 12.7) 7.36) (+ 19 5.3)))
	    (rj45-connectors *frontplate-depth*)
	    (with-transform ((translation-matrix 172 0))
	      (rj45-connectors *frontplate-depth*)))

	  (with-transform ((translation-matrix 375 0))
;;	    (drill :x 0 :y 24 :diameter 8 :depth *frontplate-depth*)
;;	    (drill :x 15 :y 24 :diameter 8 :depth *frontplate-depth*)
;;	    (drill :x 30 :y 24 :diameter 5 :depth *frontplate-depth*)
	    (drill :x 40 :y 24 :diameter 5 :depth *frontplate-depth*)
	    ))))))
    
(defun ayce-back-platte ()
  (let ((tool *alu-tool-ayce*)
	(*backplate-depth* 2))
    (with-program ("backplatte")
      (with-named-pass ("umrandung")
	(goto-abs :x 0 :y 0)
	(rectangle 435 44))
      (with-named-pass ("drills")
	(with-tool (tool)
	  (goto-abs :x 0 :y 0)
	  (goto-abs :z 30)
	  (goto-abs :x 35 :y 22)
	  (goto-abs :z *fly-height*)
	  (drill :x 35 :y 22 :diameter 20 :depth *backplate-depth*)
	  (drill :x 65 :y 22 :diameter 16 :depth *backplate-depth*))))))

  