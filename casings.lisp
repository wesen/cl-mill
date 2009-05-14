.(in-package :gcode)

(defun hammond-29830-psla ()
  (with-program ("hammond-29830")
    (with-tool ((make-instance 'tool
			       :diameter 2
			       :number 6
			       :feed-xy 600
			       :feed-z 240
			       :depth 3))

      (spindle-on)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)

      (with-named-pass ("outline")
	(goto-abs :x 0 :y 0)
	(rectangle-inline 95 120 :depth 10)
	(goto-abs :x 2 :y 2)
	(rectangle-inline (- 95 4) (- 120 4) :depth 10))

      (with-named-pass ("fix")

	(goto-abs :x 0 :y 0)
	(goto-abs :y 119.2 :x 0)
	(with-tool-down (3)
	  (mill-abs :y 119.2 :x 95))
	(with-tool-down (6)
	  (mill-abs :y 119.2 :x 0))
	(with-tool-down (10)
	  (mill-abs :y 119.2 :x 95))
	
	(goto-abs :y (- 120 9) :x 95)
	(with-tool-down (3)
	  (mill-abs :y (- 120 9) :x 86))
	(with-tool-down (6)
	  (mill-abs :y (- 120 9) :x 95))
	(goto-abs :y (- 120 9) :x 0)
	(with-tool-down (3)
	  (mill-abs :y (- 120 9) :x 9))
	(with-tool-down (6)
	  (mill-abs :y (- 120 9) :x 0))

	(goto-abs :x 0 :y 0)
	(rectangle-inline (- 95 4) (- 120 4) :depth 1)

	
      )
      
      (with-named-pass ("inner-rectangles")

	(goto-abs :y 0         :x 0)
	(rectangle-mill 9 9 :depth 6)

	(goto-abs :y (- 120 9) :x 0)
	(rectangle-mill 9 9 :depth 6)

	(goto-abs :y (- 120 9) :x (- 95 9))
	(rectangle-mill 9 9 :depth 6)

	(goto-abs :y 0         :x (- 95 9))
	(rectangle-mill 9 9 :depth 6))

      (with-named-pass ("drills")
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
	
	(drill :y 4.5         :x 4.5        :diameter 3 :depth 16)
	(drill :y (- 120 4.5) :x 4.5        :diameter 3 :depth 16)
	(drill :y (- 120 4.5) :x (- 95 4.5) :diameter 3 :depth 16) 
	(drill :y 4.5         :x (- 95 4.5) :diameter 3 :depth 16)))))
  

;; X, Y coordnates test brett
;; 42.9, 20.6 -> 2, 2


;; mini command woehr
;; http://www.industriegehaeuse.woehrgmbh.de/assets/pdf/GH02AL002-010-030_060-110_150.pdf
;; distance between drills: 109.5, 84.2
;; outer dimensions: 119.0, 93.5, 34.0

(defparameter *mdf-tool-2mm*
  (make-instance 'tool
		 :diameter 2
		 :number 10
		 :depth 3))
		 

(defun woehr-gehauese-090 ()
  (with-tool (*mdf-tool-2mm*)
    (spindle-on)
    (goto-abs :x 0 :y 0)
    (goto-abs :z *fly-height*)
    
    (with-named-pass ("outline")
  (with-tool (*mdf-tool-2mm*)
      (goto-abs :x 0 :y 0)
      (rectangle-inline 93.5 119 :depth 1)
      (goto-abs :x 0.7 :y 0.7)
      (rectangle-inline (- 93.5 1.4) (- 119 1.4) :depth 1)))
    
    (with-named-pass ("drills")
  (with-tool (*mdf-tool-2mm*)
      (goto-abs :x 0 :y 0)
      (goto-abs :z *fly-height*)
      
      (drill :y 5         :x 4.65        :diameter 3.5 :depth 12.5)
      (drill :y 5         :x (- 93.5 4.65) :diameter 3.5 :depth 12.5)
      (drill :y (- 119 5) :x (- 93.5 4.65) :diameter 3.5 :depth 12.5) 
      (drill :y (- 119 5) :x 4.65        :diameter 3.5 :depth 12.5)

      ))))

(defun woehr-gehauese-090-rueckwaerts-drills ()
  (with-tool (*mdf-tool-2mm*)
    (spindle-on)
    (goto-abs :x 0 :y 0)
    (goto-abs :z *fly-height*)
    
    (with-named-pass ("drills")
      (with-tool (*mdf-tool-2mm*)
	(goto-abs :x 0 :y 0)
	(goto-abs :z *fly-height*)

	(with-transform ((translation-matrix -4.65 -5))
	  (drill :y 5         :x 4.65        :diameter 7 :depth 11)
	  (drill :y 5         :x (- 93.5 4.65) :diameter 7 :depth 11)
	  (drill :y (- 119 5) :x (- 93.5 4.65) :diameter 7 :depth 11) 
	  (drill :y (- 119 5) :x 4.65        :diameter 7 :depth 11)
	  
	  )))))


(defun woehr-090-program ()
  (with-program ("woehr")
    (woehr-gehauese-090)
    (with-transform ((translation-matrix 0 122))
      (woehr-gehauese-090))
    (with-transform ((translation-matrix 0 244))
      (woehr-gehauese-090))))

(defun woehr-090-platte ()
  (with-program ("woehr")
    (with-named-pass ("mill")
      (with-tool (*mdf-tool-2mm*)
	(woehr-gehauese-090)
	(with-transform ((translation-matrix 0 122))
	  (woehr-gehauese-090))
	(with-transform ((translation-matrix 0 244))
	  (woehr-gehauese-090))
	
	(with-transform ((translation-matrix 105 0))
	  (woehr-gehauese-090)
	  (with-transform ((translation-matrix 0 122))
	    (woehr-gehauese-090))
	  (with-transform ((translation-matrix 0 244))
	    (woehr-gehauese-090)))
	
	(with-transform ((translation-matrix 210 0))
	  (woehr-gehauese-090)
	  (with-transform ((translation-matrix 0 122))
	    (woehr-gehauese-090))
	  (with-transform ((translation-matrix 0 244))
	    (woehr-gehauese-090)))
	
	(with-transform ((translation-matrix 315 0))
	  (woehr-gehauese-090)
	  (with-transform ((translation-matrix 0 122))
	    (woehr-gehauese-090))
	  (with-transform ((translation-matrix 0 244))
	    (woehr-gehauese-090)))))))
  
  
  
(defun woehr-090-platte-rueckwaerts ()
  (with-program ("woehr")
    (with-named-pass ("mill")
      (with-tool (*mdf-tool-2mm*)
	(woehr-gehauese-090-rueckwaerts-drills)
	(with-transform ((translation-matrix 0 122))
	  (woehr-gehauese-090-rueckwaerts-drills))
	(with-transform ((translation-matrix 0 244))
	  (woehr-gehauese-090-rueckwaerts-drills))
	
	(with-transform ((translation-matrix 105 0))
	  (woehr-gehauese-090-rueckwaerts-drills)
	  (with-transform ((translation-matrix 0 122))
	    (woehr-gehauese-090-rueckwaerts-drills))
	  (with-transform ((translation-matrix 0 244))
	    (woehr-gehauese-090-rueckwaerts-drills)))
	
	(with-transform ((translation-matrix 210 0))
	  (woehr-gehauese-090-rueckwaerts-drills)
	  (with-transform ((translation-matrix 0 122))
	    (woehr-gehauese-090-rueckwaerts-drills))
	  (with-transform ((translation-matrix 0 244))
	    (woehr-gehauese-090-rueckwaerts-drills)))
	
	(with-transform ((translation-matrix 315 0))
	  (woehr-gehauese-090-rueckwaerts-drills)
	  (with-transform ((translation-matrix 0 122))
	    (woehr-gehauese-090-rueckwaerts-drills))
	  (with-transform ((translation-matrix 0 244))
	    (woehr-gehauese-090-rueckwaerts-drills)))))))
  
  
  