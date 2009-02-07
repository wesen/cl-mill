(in-package :gcode)


;; command line for dynamic lib variant:
;; gcc -dynamiclib -flat_namespace -undefined suppress -o libpotrace.dylib curve.o trace.o decompose.o potracelib.o

(def-struct potrace-progress
  (callback     :pointer-void)
  (data         :pointer-void)
  (min          :double)
  (max          :double)
  (epsilon      :double))

(def-struct potrace-param
  (turdsize     :int)
  (turnpolicy   :int)
  (alphamax     :double)
  (opticurve    :int)
  (opttolerance :double)
  (progress     (* potrace-progress)))

(def-foreign-type potrace-word :unsigned-long)

(def-foreign-type potrace-word-ptr (* potrace-word))

(def-struct potrace-bitmap
  (width        :int)
  (height       :int)
  (dy           :int)
  (map          (* potrace-word)))

(def-struct potrace-dpoint
  (x            :double)
  (y            :double))

(defvar +POTRACE-CURVETO+ 1)
(defvar +POTRACE-CORNER+  2)

(def-struct potrace-curve
  (n            :int)
  (tag          (* :int))
  (c            (* (:array potrace-dpoint 3))))

(def-struct potrace-path
  (area         :int)
  (sign         :int)
  (curve        potrace-curve)
  (next         :pointer-self)
  (childlist    :pointer-self)
  (priv         :pointer-void))

(defvar +POTRACE-STATUS-OK+ 0)
(defvar +POTRACE-STATUS-INCOMPLETE+ 1)

(def-struct potrace-state
  (status       :int)
  (plist        (* potrace-path))
  (priv         :pointer-void))

(def-function ("potrace_param_default" potrace-param-default)
    ()
  :returning (* potrace-param)
  :module "potrace")

(def-function ("potrace_param_free" potrace-param-free)
    ((p (* potrace-param)))
  :returning :void
  :module "potrace")

(def-function ("potrace_trace" potrace-trace)
    ((param  (* potrace-param))
     (bitmap (* potrace-bitmap)))
  :returning (* potrace-state)
  :module "potrace")

(def-function ("potrace_state_free" potrace-state-free)
    ((st (* potrace-state)))
  :returning :void
  :module "potrace")

(def-function ("potrace_version" potrace-version)
    ()
  :returning :cstring
  :module "potrace")

(def-function ("potrace_set_pixel" potrace-set-pixel)
    ((bitmap (* potrace-bitmap))
     (i       :unsigned-short)
     (j       :unsigned-short))
  :returning :void
  :module "potrace")

(def-function ("potrace_clear_pixel" potrace-clear-pixel)
    ((bitmap (* potrace-bitmap))
     (i       :unsigned-short)
     (j       :unsigned-short))
  :returning :void
  :module "potrace")

(def-function ("potrace_get_pixel" potrace-get-pixel)
    ((bitmap (* potrace-bitmap))
     (i       :unsigned-short)
     (j       :unsigned-short))
  :returning :unsigned-char
  :module "potrace")

(defun convert-test ()
  (with-image-from-file* ("/Users/manuel/board-normal-5.png")
    (do-rows (y)
      (do-pixels-in-row (x)
	(unless (= (raw-pixel) #xFFFFFF)
	  (format t "Pixel <~A,~A> has value ~X~%" x y (raw-pixel)))))))

(defvar +N+ 32)

(def-array-pointer test (* potrace-dpoint ))

(defun get-point (c i j)
  (deref-array (deref-array c '(* (:array potrace-dpoint 3)) i) '(:array potrace-dpoint 3) j))

(defun pot-round (num)
;;  (format t "num: ~A num * 1000: ~A~%" num (* num 1000.0))
  (if (or (sb-ext:float-nan-p num)
	  (sb-ext:float-infinity-p (* 1000.0 num))
	  (> num most-positive-short-float)
	  (< num most-negative-short-float))
      num
      (/ (round (* num 1000.0))
	 1000.0)))

(defun potrace-to-curve (curve)
  (let* ((tags (get-slot-value curve 'potrace-curve 'tag))
	 (c (get-slot-pointer curve 'potrace-curve 'c))
	 (n (get-slot-value curve 'potrace-curve 'n)))
    
    (format t "n: ~A~%" n)
    (let* ((p (get-point c (1- n) 2))
	  (a (2dp (get-slot-value p 'potrace-dpoint 'x)
		   (get-slot-value p 'potrace-dpoint 'y)))
	   res)

      (loop for i from 0 below n
	   for tag = (deref-array tags '(* :int) i)
	   do (let* ((p1 (get-point c i 0))
		     (p1x (pot-round (get-slot-value p1 'potrace-dpoint 'x)))
		     (p1y (pot-round (get-slot-value p1 'potrace-dpoint 'y)))
		     (p1p (2dp p1x p1y))
		     (p2 (get-point c i 1))
		     (p2x (pot-round (get-slot-value p2 'potrace-dpoint 'x)))
		     (p2y (pot-round (get-slot-value p2 'potrace-dpoint 'y)))
		     (p2p (2dp p2x p2y))
		     (p3 (get-point c i 2))
		     (p3x (pot-round (get-slot-value p3 'potrace-dpoint 'x)))
		     (p3y (pot-round (get-slot-value p3 'potrace-dpoint 'y)))
		     (p3p (2dp p3x p3y))
		     )
;;		(format t "~A: " i)
		(case tag
		  (1
;;		   (format t "CURTO ~A ~A ~A~%" p1p p2p p3p)
		   (push (make-bezier :a a :u p1p :v p2p :b p3p) res))
		  (2
;;		   (format t "CORNER ~A ~A~%" p2p p3p)
		   (push (make-line :a a :b p2p) res)
		   (push (make-line :a p2p :b p3p) res)))
		(setf a p3p)))
      (nreverse res)

	)))

(defun potrace-to-curves (path)
  (let (res)
    (do* ((pth path (get-slot-pointer pth 'potrace-path 'next)))
	 ((null-pointer-p pth)
	  (nreverse res))
      (let ((curve (get-slot-pointer pth 'potrace-path 'curve)))
	(push (potrace-to-curve curve) res)))))

(defun image-colors (image)
  (let ((colors))
    (with-image-from-file* (image)
      (do-rows (y)
	(do-pixels-in-row (x)
	  (pushnew (raw-pixel) colors))))
    (format t "colors: ")
    (dolist (c colors)
      (format t "~X " c))
    (format t "~%")
    colors))
	

(defun trace-image (image &key (colors (list #x000000)) (not-colors))
  (let (res)
  (with-image-from-file* (image)
    (let* ((width (image-width))
	   (height (image-height))
	   (dy  (ceiling (/ width 32)))
	   ;; XXX memory bla?
	   (raw-array (allocate-foreign-object 'potrace-word (* dy (+ 20 height)))))

      (format t "dy ~A height ~A~%" dy height)
      
      (with-foreign-object (bitmap 'potrace-bitmap)
	(setf (get-slot-value bitmap 'potrace-bitmap 'width) width
	      (get-slot-value bitmap 'potrace-bitmap 'height) height
	      (get-slot-value bitmap 'potrace-bitmap 'dy) dy
	      (get-slot-value bitmap 'potrace-bitmap 'map) raw-array)
	(do-rows (y)
	  (do-pixels-in-row (x)
	    ;; invert coordinates for Y
;;	    (format t "~A,~A raw-pixel ~X~%" x y (raw-pixel))
	    (if (null not-colors)
		(if (not (member (raw-pixel) colors))
		    (potrace-clear-pixel bitmap x (- (image-height) y))
		    (potrace-set-pixel bitmap x (- (image-height) y)))
		(if (member (raw-pixel) not-colors)
		    (potrace-clear-pixel bitmap x (- (image-height) y))
		    (potrace-set-pixel bitmap x (- (image-height) y))))

	    #+nil(if (= (raw-pixel) #xFFFFFF)
		(potrace-clear-pixel bitmap x y)
		(potrace-set-pixel bitmap x y))
	    
;;	    (format t "~A" (potrace-get-pixel bitmap x y))
	    )
;;	  (format t "~%")
	  )
	(format t "tracing~%")
	
	(let* ((param (potrace-param-default))
	       (state (potrace-trace param bitmap)))
	  (if (= (get-slot-value state 'potrace-state 'status) +POTRACE-STATUS-OK+)
	      (progn
		(format t "trace ok~%")
		(let ((path (get-slot-pointer state 'potrace-state 'plist)))

		  (setf res (potrace-to-curves path))

		  ))
	      (format t "trace incomplete~%"))
	  (potrace-state-free state)
	  (potrace-param-free param)))
      (free-foreign-object raw-array)))
  res))
	

(defun test ()
    (let* ((width 20)
	   (height 20)
	   (dy (1+ (ceiling (/ width 32))))
	   (raw-array (allocate-foreign-object 'potrace-word (* dy height))))
      (unwind-protect
	   (with-foreign-object (bitmap 'potrace-bitmap)
	     (setf (get-slot-value bitmap 'potrace-bitmap 'width) width
		   (get-slot-value bitmap 'potrace-bitmap 'height) height
		   (get-slot-value bitmap 'potrace-bitmap 'dy) dy
		   (get-slot-value bitmap 'potrace-bitmap 'map) raw-array)
	     (dotimes (i width)
	       (dotimes (j height)
		 (if (oddp i)
		     (potrace-set-pixel bitmap i j)
		     (potrace-clear-pixel bitmap i j))))
	     (dotimes (i width)
	       (dotimes (j height)
		 (format t "~A,~A: ~A~%" i j (potrace-get-pixel bitmap i j)))))
	(free-foreign-object raw-array))))
		  

(defun test2 ()
    (let* ((width 80)
	   (height 80)
	   (dy (ceiling (/ width 32)))
	   (raw-array (allocate-foreign-object 'potrace-word (* dy height)))
	   (res))
      (format t "dy: ~A~%" dy)
      (unwind-protect
	   (with-foreign-object (bitmap 'potrace-bitmap)
	     (setf (get-slot-value bitmap 'potrace-bitmap 'width) width
		   (get-slot-value bitmap 'potrace-bitmap 'height) height
		   (get-slot-value bitmap 'potrace-bitmap 'dy) dy
		   (get-slot-value bitmap 'potrace-bitmap 'map) raw-array)
	     (dotimes (i width)
	       (dotimes (j height)
		 (potrace-clear-pixel bitmap i j)
		 (when (and (> i 30) (> j 30)
			    (< i 70) (< j 70))
		   (potrace-set-pixel bitmap i j))))
	     (let* ((param (potrace-param-default))
		    (state (potrace-trace param bitmap)))
	       (if (= (get-slot-value state 'potrace-state 'status) +POTRACE-STATUS-OK+)
		   (progn
		     (format t "trace ok~%")
		     (let ((path (get-slot-pointer state 'potrace-state 'plist)))
		       (push (print-curve (get-slot-pointer path 'potrace-path 'curve)) res)
		       (let ((next (get-slot-pointer path 'potrace-path 'next)))
			 (unless (null-pointer-p next)
			   (push (print-curve (get-slot-pointer path 'potrace-path 'curve)) res)))))
		   (format t "trace incomplete~%"))
	       (potrace-state-free state)
	       (potrace-param-free param)))
	(free-foreign-object raw-array))
      (nreverse res)))

