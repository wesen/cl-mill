(in-package :gcode)

(defclass pass ()
  ((name :initarg :name :initform "" :reader pass-name)
   (current-x :initarg :current-x :initform 0 :accessor pass-current-x)
   (min-x :initarg :min-x :initform MOST-POSITIVE-FIXNUM :accessor pass-min-x)
   (max-x :initarg :max-x :initform 0 :accessor pass-max-x)
   (current-y :initarg :current-y :initform 0 :accessor pass-current-y)
   (min-y :initarg :min-y :initform MOST-POSITIVE-FIXNUM :accessor pass-min-y)
   (max-y :initarg :max-y :initform 0 :accessor pass-max-y)
   (current-z :initarg :current-z :initform 0 :accessor pass-current-z)
   (min-z :initarg :min-z :initform MOST-POSITIVE-FIXNUM :accessor pass-min-z)
   (max-z :initarg :max-z :initform 0 :accessor pass-max-z)
   (moves :initarg :moves :initform nil :accessor pass-raw-moves)))

(defmethod print-object ((pass pass) stream)
  (print-unreadable-object (pass stream :type t :identity nil)
    (with-slots (name
		 current-x min-x max-x
		 current-y min-y max-y
		 current-z min-z max-z) pass
      (format stream "~S current: ~A,~A,~A min: ~A,~A,~A max: ~A,~A,~A"
	      name current-x current-y current-z
	      min-x min-y min-z
	      max-x max-y max-z))))

(defmethod pass-moves ((pass pass))
  (reverse (pass-raw-moves pass)))

(defmethod (setf pass-moves) (list (pass pass))
  (setf (pass-raw-moves pass) (reverse list)))

(defvar *current-pass*)

(defun current-x ()
  (pass-current-x *current-pass*))
(defun orig-current-x ()
  (first (transform-vector (pass-current-x *current-pass*) (pass-current-y *current-pass*)
			   (invert-matrix *current-transform*))))
(defun (setf current-x) (val)
  (setf (pass-current-x *current-pass*) val))
(defun min-x ()
  (pass-min-x *current-pass*))
(defun (setf min-x) (val)
  (setf (pass-min-x *current-pass*) val))
(defun max-x ()
  (pass-max-x *current-pass*))
(defun (setf max-x) (val)
  (setf (pass-max-x *current-pass*) val))

(defun current-y ()
  (pass-current-y *current-pass*))
(defun orig-current-y ()
  (second (transform-vector (pass-current-x *current-pass*) (pass-current-y *current-pass*) 
			   (invert-matrix *current-transform*))))
(defun (setf current-y) (val)
  (setf (pass-current-y *current-pass*) val))
(defun min-y ()
  (pass-min-y *current-pass*))
(defun (setf min-y) (val)
  (setf (pass-min-y *current-pass*) val))
(defun max-y ()
  (pass-max-y *current-pass*))
(defun (setf max-y) (val)
  (setf (pass-max-y *current-pass*) val))

(defun current-z ()
  (pass-current-z *current-pass*))
(defun (setf current-z) (val)
  (setf (pass-current-z *current-pass*) val))
(defun min-z ()
  (pass-min-z *current-pass*))
(defun (setf min-z) (val)
  (setf (pass-min-z *current-pass*) val))
(defun max-z ()
  (pass-max-z *current-pass*))
(defun (setf max-z) (val)
  (setf (pass-max-z *current-pass*) val))

(defmacro with-new-pass ((name) &rest body)
  `(let ((*current-pass* (make-instance 'pass :name ,name)))
     ,@body))

(defun find-named-pass (name)
  (find name (current-passes) :key #'pass-name :test #'string-equal))

(defmacro with-named-pass ((name) &rest body)
  (let ((pass (gensym)))
    `(let ((,pass (find-named-pass ,name)))
       (when (null ,pass)
	 (setf ,pass (make-instance 'pass :name ,name))
	 (push ,pass (current-passes)))
       (let ((*current-pass* ,pass))
	   ,@body))))


(defclass gcode-program ()
  ((passes :initarg :passes :initform nil :accessor gcode-program-passes)
   (name :initarg :name :initform "" :accessor gcode-program-name)))

(defmethod print-object ((program gcode-program) stream)
  (print-unreadable-object (program stream :type t)
    (with-slots (name passes) program
      (format stream "~S~%  passes:~%    ~A" name passes))))

(defun program-min-x (&optional (program *current-program*))
  (let ((passes (gcode-program-passes program)))
    (reduce #'min (mapcar #'pass-min-x passes))))

(defun program-min-y (&optional (program *current-program*))
  (let ((passes (gcode-program-passes program)))
    (reduce #'min (mapcar #'pass-min-y passes))))

(defun program-min-z (&optional (program *current-program*))
  (let ((passes (gcode-program-passes program)))
    (reduce #'min (mapcar #'pass-min-z passes))))

(defun program-max-x (&optional (program *current-program*))
  (let ((passes (gcode-program-passes program)))
    (reduce #'max (mapcar #'pass-max-x passes))))

(defun program-max-y (&optional (program *current-program*))
  (let ((passes (gcode-program-passes program)))
    (reduce #'max (mapcar #'pass-max-y passes))))

(defun program-max-z (&optional (program *current-program*))
  (let ((passes (gcode-program-passes program)))
    (reduce #'max (mapcar #'pass-max-z passes))))

(defvar *current-program*)

(defmacro with-program ((name) &rest body)
  `(let ((*current-program* (make-instance 'gcode-program :name ,name)))
     (with-named-pass ("mill")
       ,@body
       *current-program*)))

(defun current-passes ()
  (gcode-program-passes *current-program*))

(defun (setf current-passes) (val)
  (setf (gcode-program-passes *current-program*) val))

(defmethod program-pass ((program gcode-program) name)
  (find name (gcode-program-passes program) :key #'pass-name :test #'string-equal))

(defun pass-file-name (file pass)
  (make-pathname :defaults file :name (format nil "~A-~A" (pathname-name file) (pass-name pass))))

(defmethod program-to-file ((program gcode-program) file &key order)
  (unless order
    (setf order (mapcar #'pass-name (gcode-program-passes program))))
  (unless (pathname-name file)
    (setf file (make-pathname :defaults file :name (gcode-program-name program)
			      :type "nc")))
  (let ((all-moves (loop for name in order
		      for pass = (program-pass program name)
		      for moves = (when pass (cons `(:m03) (pass-moves pass)))
		      for pass-filename = (when pass (pass-file-name file pass))
		      collect moves)))
    (g-to-file all-moves file)
    (format t "saved ~A to ~A~%" order file))
  (let ((all-passes (loop for pass in (gcode-program-passes program)
		      for moves = (cons `(:m03) (pass-moves pass))
		      for pass-filename = (pass-file-name file pass)
		      do (g-to-file moves pass-filename)
		      do (format t "saved ~A to ~A~%" (pass-name pass) pass-filename)
		      collect moves)))))
	 


(defmethod program-pass-moves ((program gcode-program) name)
  (pass-moves (program-pass program name)))
       

;; coordinate variables
#|
(defvar *current-x* 0)
(defvar *current-y* 0)
(defvar *current-z* 0)

(defvar *max-x* 0)
(defvar *max-y* 0)
(defvar *max-z* 0)

(defvar *min-x* MOST-POSITIVE-FIXNUM)
(defvar *min-y* MOST-POSITIVE-FIXNUM)
(defvar *min-z* MOST-POSITIVE-FIXNUM)
|#

;; tool definition

(defclass tool ()
  ((diameter :initarg :diameter :initform 1 :reader tool-diameter) ;; mm
   (feed-xy :initarg :feed-xy :initform 10 :reader tool-feed-xy) ;; mm / min
   (feed-z :initarg :feed-z :initform 5 :reader tool-feed-z) ;; mm / min
   (depth :initarg :depth :initform 3 :reader tool-depth) ;; mm
   (number :initarg :number :initform 1 :reader tool-number)))

(defparameter *test-tool*
  (make-instance 'tool
		 :diameter 2
		 :feed-xy 10
		 :feed-z 10
		 :depth 1))

(defparameter *default-tool*
  (make-instance 'tool))

(defvar *current-tool*
  *default-tool*)

(defmacro with-tool ((tool) &rest body)
  `(let ((*current-tool* ,tool))
     (switch-tool (tool-number ,tool))
     ,@body))

(defun add-gcode-move (move)
  (push move (pass-raw-moves *current-pass*))
  nil)

(defun wait (seconds)
  (add-gcode-move `(:g04 (:h ,seconds))))

(defun make-keyword (string)
  (intern (string-upcase string) (find-package :keyword)))

(defun switch-tool (number)
  (add-gcode-move (list (make-keyword (format nil "T~A" number)))))

;;; transform movements

;; movement commands

(defun comment (cmt)
  (add-gcode-move `((:|| ,(format nil "(~A)" cmt)))))

(defun update-current-coords (x y z)
  (when x
    (setf (current-x) x))
  (when y
    (setf (current-y) y))
  (when z
    (setf (current-z) z))
  (when (> (current-x) (max-x))
    (setf (max-x) (current-x)))
  (when (> (current-y) (max-y))
    (setf (max-y) (current-y)))
  (when (> (current-z) (max-z))
    (setf (max-z) (current-z)))
  (when (< (current-x) (min-x))
    (setf (min-x) (current-x)))
  (when (< (current-y) (min-y))
    (setf (min-y) (current-y)))
  (when (< (current-z) (min-z))
    (setf (min-z) (current-z)))
  )
  
(defun goto (&key x y z cmt)
  (let ((vec (transform-vector (or x (orig-current-x))
			       (or y (orig-current-y))
			       *current-transform*)))
    (update-current-coords (first vec) (second vec) z)
    (add-gcode-move `(:G00 ,@(when x `((:x ,(first vec))))
			   ,@(when y `((:y ,(second vec))))
			   ,@(when z `((:z ,z)))
			   ,@(when cmt (comment cmt))))))

(defun mill (&key x y z f cmt)
  (unless f
    (when *current-tool*
      (setf f (tool-feed-xy *current-tool*))))
  (let ((vec (transform-vector (or x (orig-current-x))
			       (or y (orig-current-y))
			       *current-transform*)))
    (update-current-coords (first vec) (second vec) z)
    (add-gcode-move `(:G01 ,@(unless (null x) `((:x ,(first vec))))
			   ,@(unless (null y) `((:y ,(second vec))))
			   ,@(unless (null z) `((:z ,z)))
			   ,@(unless (null f) `((:f ,f)))
			   ,@(when cmt (comment cmt))))))

(defun arc-cw (&key x y z i j k f)
  (let ((vec (transform-vector (or x (orig-current-x))
			       (or y (orig-current-y))
			       *current-transform*))
	(vec2 (transform-vector (or i (orig-current-x))
				(or j (orig-current-y))
				*current-transform*)))
    ;; XXX bounding box better
    (update-current-coords (first vec) (second vec) z)
    (add-gcode-move `(:G02 ,@(when x `((:x ,(first vec))))
			   ,@(when y `((:y ,(second vec))))
			   ,@(when z `((:z ,z)))
			   ,@(when i `((:i ,(first vec2))))
			   ,@(when j `((:j ,(second vec2))))
			   ,@(when k `((:k ,k)))
			   ,@(when f `((:f ,f))))
	   
	 )))

(defun arc-ccw (&key x y z i j k f)
  (let ((vec (transform-vector (or x (orig-current-x))
			       (or y (orig-current-y))
			       *current-transform*))
	(vec2 (transform-vector (or i (orig-current-x))
				(or j (orig-current-y))
				*current-transform*)))
    (update-current-coords (first vec) (second vec) z)
    (add-gcode-move `(:G03 ,@(when x `((:x ,(first vec))))
			   ,@(when y `((:y ,(second vec))))
			   ,@(when z `((:z ,z)))
			   ,@(when i `((:i ,(first vec2))))
			   ,@(when j `((:j ,(second vec2))))
			   ,@(when k `((:k ,k)))
			   ,@(when f `((:f ,f)))
			   ))))

(defun goto-abs (&key x y z)
  (goto :x x :y y :z z))

(defun goto-rel (&key (x 0) (y 0) (z 0))
  (goto-abs :x (+ (orig-current-x) x)
	    :y (+ (orig-current-y) y)
	    :z (+ (current-z) z)))

(defun mill-abs (&key x y z f)
  (mill :x x :y y :z z :f f))

(defun mill-rel (&key (x 0) (y 0) (z 0) f)
  (mill-abs :x (+ (orig-current-x) x)
	    :y (+ (orig-current-y) y)
	    :z (+ (current-z) z)
	    :f f))

;; arcs

(defun arc-cw-abs (&key x y z i j k f)
  (arc-cw :x x :y y :z z :i i :j j :k k :f f))

(defun arc-cw-rel (&key (x (orig-current-x)) (y (orig-current-y)) (z 0) (i 0) (j 0) (k 0) f)
  (arc-cw-abs :x (+ (orig-current-x) x)
	      :y (+ (orig-current-y) y)
	      :z (+ (current-z) z)
	      :i (+ (orig-current-x) i)
	      :j (+ (orig-current-y) j)
	      :k (+ (current-z) k)
	      :f f))

(defun arc-ccw-abs (&key x y z i j k f)
  (arc-ccw :x x :y y :z z :i i :j j :k k :f f))

(defun arc-ccw-rel (&key x y z i j k f)
  (arc-ccw-abs  :x (+ (orig-current-x) x)
		:y (+ (orig-current-y) y)
		:z (+ (current-z) z)
		:i (+ (orig-current-x) i)
		:j (+ (orig-current-y) j)
		:k (+ (current-z) k)
		:f f))

(defmacro with-rel-back-xy (() &rest body)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,x (orig-current-x))
	   (,y (orig-current-y)))
;;       (format t "x before: ~A, y before: ~A~%" ,x ,y)
       ,@body
       (format t "x after ~A, y after: ~A~%" (orig-current-x) (orig-current-y))
       (goto-rel :x (- ,x (orig-current-x))
		 :y (- ,y (orig-current-y))))))
	  

;; CORRECTION is:
;; - middle - just follow outline with tool
;; - outer
;; - inner

(defmacro repeat-for-depth ((depth) &rest body)
  (let ((curx (gensym))
	(cury (gensym)))
  `(let ((nums (/ ,depth (tool-depth *current-tool*))))
     (if (> nums 1)
	 (progn
	   (loop for i from 0 below nums
	      for curdepth from (tool-depth *current-tool*) by (tool-depth *current-tool*)
	      do (tool-down :depth (min ,depth curdepth))
		(let ((,curx (orig-current-x))
		      (,cury (orig-current-y)))
		  ,@body
		  (unless (and (epsilon-= ,curx (orig-current-x))
			       (epsilon-= ,cury (orig-current-y)))
		    (tool-up)
		    (goto-abs :x ,curx :y ,cury))))
	   (tool-up))
	 
	 (with-tool-down (,depth)
	   ,@body)))))

(defparameter *fly-height* 2.5)

(defun spindle-on ()
  (add-gcode-move `(:m3)))

(defun home ()
  (goto-abs :x 0 :y 0)
  (spindle-on))

(defmacro with-save-xy (() &rest body)
  (let ((oldx (gensym "OLDX"))
	(oldy (gensym "OLDY")))
  `(let ((,oldx (current-x))
	 (,oldy (current-y)))
     (unwind-protect
	  (progn
	    ,@body)
       (setf (current-x) ,oldx
	     (current-y) ,oldy)))))

(defmacro with-current-xy ((x y) &rest body)
  `(with-save-xy ()
     (setf (current-x) ,x
	   (current-y) ,y)
     ,@body))

(defmacro repeat ((i) &rest body)
  (let ((bla (gensym)))
    `(loop for ,bla from 1 to ,i
	  do (progn ,@body))))


;; tool commands

(defun pump-on ()
  (add-gcode-move `(:m8)))

(defun program-stop ()
  (add-gcode-move `(:m00)))

(defun pump-off ()
  (add-gcode-move `(:m9)))

(defun tool-down (&key depth)
  (pump-on)
  (mill-abs :z (if depth
		   (- depth)
		   (- (tool-depth *current-tool*)))
	    :f (tool-feed-z *current-tool*)))

(defun tool-up ()
  (goto-abs :z *fly-height*)
  (pump-off))

(defmacro with-tool-down ((&optional depth) &rest body)
  `(progn
     (tool-down :depth ,depth)
     ,@body
     (tool-up)))

(defun fly-to (&key (x (orig-current-x)) (y (orig-current-y)))
  (let ((z (current-z)))
    (unless (and (= (orig-current-x) x)
		 (= (orig-current-y) y))
      (tool-up)
      (goto-abs :x x :y y)
      (tool-down :depth z))))