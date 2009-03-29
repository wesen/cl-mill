(in-package :gcode)

(defclass panel ()
  ((gcode :initarg :gcode :initform nil :reader panel-gcode)
   (min-x :initarg :min-x :initform 0 :reader panel-min-x)
   (max-x :initarg :max-x :initform 0 :reader panel-max-x)
   (min-y :initarg :min-y :initform 0 :reader panel-min-y)
   (max-y :initarg :max-y :initform 0 :reader panel-max-y)
   (min-z :initarg :min-z :initform 0 :reader panel-min-z)
   (max-z :initarg :max-z :initform 0 :reader panel-max-z)
   (name  :initarg :name  :initform "" :reader panel-name)
   (code  :initarg :code  :initform nil :reader panel-code)))

(defmethod print-object ((obj panel) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (min-x max-x min-y max-y min-z max-z name) obj
      (format stream "~S (~A-~A, ~A-~A, ~A-~A)"
	      name min-x max-x min-y max-y min-z max-z))))

(defmacro with-panel ((name) &rest body)
  (let ((res (gensym)))
  `(with-new-pass (,name)
     (let ((,res (g-program ,@body)))
       (make-instance 'panel :name ,name
		      :gcode ,res
		      :min-x (min-x) :max-x (max-x)
		      :min-y (min-y) :max-y (max-y)
		      :min-z (min-z) :max-z (max-z)
		      :code '(progn ,@body))))))

(defun calculate-panel (function-name)
  ;; new program XXX
  (let* ((*current-program* (make-instance 'gcode-program :name "calculate shit")))
    (funcall (symbol-function function-name))
    (let* ((passes (gcode-program-passes *current-program*))
	   (min-x (apply #'min (mapcar #'pass-min-x passes)))
	   (max-x (apply #'max (mapcar #'pass-max-x passes)))
	   (min-y (apply #'min (mapcar #'pass-min-y passes)))
	   (max-y (apply #'max (mapcar #'pass-max-y passes)))
	   (min-z (apply #'min (mapcar #'pass-min-z passes)))
	   (max-z (apply #'max (mapcar #'pass-max-z passes))))
	(make-instance 'panel :name function-name
		       :gcode res
		       :min-x min-x :max-x max-x
		       :min-y min-y :max-y max-y
		       :min-z min-z :max-z max-z
		       :code `(,function-name)))))

(defun calculate-panel-code (code &key passname)
  (let ((*current-program* (make-instance 'gcode-program :name "calculate shit")))
    (with-new-pass ("calculate pass")
      (let ((res (with-save-xy () (eval `(progn ,@code)))))
	(make-instance 'panel :name "code"
		       :gcode res
		       :min-x (program-min-x) :max-x (program-max-x)
		       :min-y (program-min-y) :max-y (program-max-y)
		       :min-z (program-min-z) :max-z (program-max-z)
		       :code `(with-named-pass (,(if passname passname "mill")) ,@code))))))

(defun calculate-panel-file (filename)
  (let ((*current-program* (make-instance 'gcode-program :name "calculate shit")))
    (with-new-pass ("calculate pass")
      (let ((res (with-save-xy () (load-file filename))))
	(make-instance 'panel :name (pathname-name filename)
		       :gcode res
		       :min-x (min-x) :max-x (max-x)
		       :min-y (min-y) :max-y (max-y)
		       :min-z (min-z) :max-z (max-z)
		       :code `(load-file ,filename))))))


(defmacro with-panel ((name) &rest body)
  `(let ((*current-program* (make-instance 'gcode-program :name "calculate shit")))
     (with-new-pass ("calculate pass")
       (let ((res (with-save-xy () ,@body)))
	 (make-instance 'panel :name ,name
			:gcode res
			:min-x (min-x) :max-x (max-x)
			:min-y (min-y) :max-y (max-y)
			:min-z (min-z) :max-z (max-z)
			:code '(progn ,@body))))))
     

(defmethod panel-width ((panel panel))
  (- (panel-max-x panel) (panel-min-x panel)))

(defmethod panel-height ((panel panel))
  (- (panel-max-y panel) (panel-min-y panel)))

(defmethod schedule-panel ((panel panel) x y)
  (with-save-xy ()
    (with-transform ((translation-matrix x y))
      (mill-abs :z *fly-height*)
      (goto-abs :x 0 :y 0)
      (eval (panel-code panel)))))

(defun panels-max-width (panels)
  (reduce #'max (mapcar #'panel-width panels)))

(defun panels-max-height (panels)
  (reduce #'max (mapcar #'panel-height panels)))

(defun panels-by-nums (panels nums)
  (loop for i in nums
       collect (elt panels (1- i))))

(defun order-panels (panels order gap)
  (let ((rpanels (reverse order))
	(y 0)
	(max-x 0)
	(max-y 0)
	(schedules (list)))
    (loop for row-nums in rpanels
	 for row-cnt from 0
       for x = 0
       for row = (panels-by-nums panels row-nums)
       for height = (panels-max-height row)
       do (let ((pcoords (loop for panel in row
			    collect (list panel x y)
			    do (incf x (+ (panel-width panel) gap))
			    do (when (> x max-x)
				 (setf max-x x)))))
	    (setf schedules (append (if (evenp row-cnt)
					(reverse pcoords)
					pcoords)
				    schedules)))
       do (incf y (+ height gap))
       do (when (> y max-y)
	    (setf max-y y)))
    (format t "total dimensions: ~A - ~A~%" max-x max-y)
    (nreverse schedules)))
