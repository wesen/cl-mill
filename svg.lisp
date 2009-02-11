(in-package :gcode)

(defparameter *uris*
  '(("http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
     "/Users/manuel/cl-mill/svg-dtd/svg11.dtd")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-framework.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-framework.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-datatypes.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-datatypes.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-qname.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-qname.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-core-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-core-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-container-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-container-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-viewport-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-viewport-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-paint-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-paint-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-opacity-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-opacity-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-graphics-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-graphics-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-docevents-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-docevents-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-graphevents-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-graphevents-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-animevents-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-animevents-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-xlink-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-xlink-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-extresources-attrib.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-extresources-attrib.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-structure.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-structure.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-conditional.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-conditional.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-image.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-image.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-style.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-style.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-shape.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-shape.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-marker.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-marker.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-text.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-text.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-profile.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-profile.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-gradient.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-gradient.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-pattern.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-pattern.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-clip.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-clip.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-mask.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-mask.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-filter.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-filter.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-cursor.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-cursor.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-hyperlink.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-hyperlink.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-view.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-view.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-script.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-script.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-animation.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-animation.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-font.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-font.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg-extensibility.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg-extensibility.mod")



    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-model.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg11-model.mod")
    ("http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-attribs.mod"
     "/Users/manuel/cl-mill/svg-dtd/svg11-attribs.mod")

    ))

(defun whitespace-p (elt)
  (and (stringp elt)
       (cl-ppcre:scan "^\\s*$" elt)))

(defun remove-whitespace (list)
  (cond ((alexandria:proper-list-p list)
	 (remove-if #'whitespace-p (mapcar #'remove-whitespace list)))
	(t list)))

(defun load-svg (svg)
    (flet ((resolver (pubid sysid)
	     (declare (ignore pubid))
	     (dolist (uri-file *uris*)
	       (let ((uri (first uri-file))
		     (file (second uri-file)))
		 (when (puri:uri= sysid (puri:parse-uri uri))
		   (return-from resolver (open file :element-type '(unsigned-byte 8))))))))
      (remove-whitespace
       (cxml:parse-file svg (cxml:make-whitespace-normalizer (cxml-xmls:make-xmls-builder))
			:entity-resolver #'resolver))))

(defun node-name (node)
  (let ((name (first node)))
    (cond ((consp name)
	   (first name))
	  (t name))))

(defun node-children (node)
  (cddr node))

(defun node-attributes (node)
  (second node))

(defun node-attribute (node name)
  (second (find name (node-attributes node) :key #'first :test #'string-equal)))

(defvar *svg-current-x* nil)
(defvar *svg-current-y* nil)
(defvar *svg-subpath-x* nil)
(defvar *svg-subpath-y* nil)
(defvar *svg-prev-ctrl-x* nil)
(defvar *svg-prev-ctrl-y* nil)

(defun mirror-point (p1 p2)
  (let ((p3 (point-- p2 p1)))
    (point-+ p2 p3)))

(defun update-svg-current (x y)
  (setf *svg-current-x* x
	*svg-current-y* y))

(defun update-svg-subpath (x y)
  (setf *svg-subpath-x* x
	*svg-subpath-y* y))

(defun update-svg-prev-ctrl (x y)
  (setf *svg-prev-ctrl-x* x
	*svg-prev-ctrl-y* y))

(defun svg-relative-x (x)
  (+ *svg-current-x* x))

(defun svg-relative-y (y)
  (+ *svg-current-y* (- y)))

(defun svg-absolute-x (x)
  x)

(defun svg-absolute-y (y)
  (- y))

(defun interpret-svg-moveto (x y)
  (update-svg-current x y)
  (update-svg-subpath x y)
  nil)

(defun interpret-svg-curveto (ux uy vx vy bx by)
  (prog1
      (make-bezier :a (2dp *svg-current-x* *svg-current-y*)
		   :u (2dp ux uy)
		   :v (2dp vx vy)
		   :b (2dp bx by))
    (update-svg-current bx by)
    (update-svg-prev-ctrl vx vy)))

(defun interpret-svg-smoothcurveto (vx vy bx by)
  (prog1
      (make-bezier :a (2dp *svg-current-x* *svg-current-y*)
		   :u (mirror-point
		       (2dp *svg-prev-ctrl-x* *svg-prev-ctrl-y*)
		       (2dp *svg-current-x* *svg-current-y*))
		   :v (2dp vx vy)
		   :b (2dp bx by))
    (update-svg-current bx by)
    (update-svg-prev-ctrl vx vy)))

(defun interpret-svg-lineto (bx by)
  (prog1 (make-line :a (2dp *svg-current-x* *svg-current-y*)
		    :b (2dp bx by))
    (update-svg-current bx by)))
  

(defun interpret-svg-path-move (cmd args)
  (cond ((string= cmd "M")
	 (interpret-svg-moveto (svg-absolute-x (first args))
			       (svg-absolute-y (second args))))

	((string= cmd "m")
	 (interpret-svg-moveto (svg-relative-x (first args))
			       (svg-relative-y (second args))))

	((string= cmd "C")
	 (interpret-svg-curveto (svg-absolute-x (first args))
				(svg-absolute-y (second args))
				(svg-absolute-x (third args))
				(svg-absolute-y (fourth args))
				(svg-absolute-x (fifth args))
				(svg-absolute-y (sixth args))))
						
	((string= cmd "c")
	 (interpret-svg-curveto (svg-relative-x (first args))
				(svg-relative-y (second args))
				(svg-relative-x (third args))
				(svg-relative-y (fourth args))
				(svg-relative-x (fifth args))
				(svg-relative-y (sixth args))))

	((string= cmd "S")
	 (interpret-svg-smoothcurveto (svg-absolute-x (first args))
				      (svg-absolute-y (second args))
				      (svg-absolute-x (third args))
				      (svg-absolute-y (fourth args))))
	((string= cmd "s")
	 (interpret-svg-smoothcurveto (svg-relative-x (first args))
				      (svg-relative-y (second args))
				      (svg-relative-x (third args))
				      (svg-relative-y (fourth args))))

	((string= cmd "L")
	 (interpret-svg-lineto (svg-absolute-x (first args))
			       (svg-absolute-y (second args))))
	
	((string= cmd "l")
	 (interpret-svg-lineto (svg-relative-x (first args))
			       (svg-relative-y (second args))))

	((string= cmd "H")
	 (interpret-svg-lineto (svg-absolute-x (first args)) *svg-current-y*))

	((string= cmd "h")
	 (interpret-svg-lineto (svg-relative-x (first args)) *svg-current-y*))

	((string= cmd "V")
	 (interpret-svg-lineto *svg-current-x* (svg-absolute-y (first args))))

	((string= cmd "v")
	 (interpret-svg-lineto *svg-current-x* (svg-relative-y (first args))))

	((string= cmd "z")
	 (interpret-svg-lineto *svg-subpath-x* *svg-subpath-y*))

	(t (format t "unknown path: ~A ~A~%" cmd args))))
	

(defun split-svg-numbers (numberstring)
  (let ((negative nil))
    (when (eql (elt numberstring 0) #\-)
      (setf negative t
	    numberstring (subseq numberstring 1)))
    (let ((result (mapcar #'parse-float (remove-whitespace (cl-ppcre:split "[, ]"
									   (cl-ppcre:regex-replace-all "-" numberstring " -"))))))
      (when negative
	(setf (first result) (- (first result))))
      result)))

(defun interpret-svg-path (node attrs children)
  (let* ((d (node-attribute node "d"))
	 (d-nowhite (cl-ppcre:regex-replace-all "\\s+" d ""))
	 (commands (remove-whitespace (cl-ppcre:split "([mMzZlLhHvVcCsSqQtTaA])" d
						      :with-registers-p t
						      :omit-unmatched-p nil)))
	 (result))
;;    (format t "path commands: ~S~%" commandS)
    (let ((*svg-current-x* 0)
	  (*svg-current-y* 0))
      (remove nil
	      (loop for (a b) on commands by #'cddr
		 collect (interpret-svg-path-move a (when b (split-svg-numbers b))))))))

(defun interpret-svg-node (node)
  (let ((name (node-name node))
	(attrs (node-attributes node))
	(children (node-children node)))
    (cond ((string= name "path")
	   (interpret-svg-path node attrs children))
	  ((string= name "g")
	   (remove nil
		   (loop for child in (node-children node)
		      appending (interpret-svg-node child))))
	  (t (format t "unknown node: ~A~%" name)))))

(defun interpret-svg (svg)
  (unless (string= (node-name svg) "svg")
    (error "not an svg file!"))
  (remove nil
	  (loop for child in (node-children svg)
	     collect (interpret-svg-node child))))
  
(defun walk-svg-node (node)
  (format t "node: ~A~%" (node-name node))
  (dolist (attribute (node-attributes node))
    (format t "attribute: ~A~%" attribute))
  (dolist (child (node-children node))
    (walk-svg-node child)))
  
(defun walk-svg-file (svg)
  (unless (string= (node-name svg) "svg")
    (error "not an svg file!"))
  (walk-svg-node svg))

(defun test-curve-svg (svg)
  (let* ((curve (curve-to-arcs (first (interpret-svg (load-svg svg)))))
	 (bbox (bounding-box curve)))
    (format t "bbox: ~A~%" bbox)
    (with-program ("svg")
      (with-tool (*default-tool*)
	(with-named-pass ("svg")
	  (goto-abs :z *fly-height*)
	  (let ((start (curve-start curve)))
	    (goto-abs :x (2d-point-x start)
		      :y (2d-point-y start)))
	  (with-tool-down ()
	      (mill-curve curve)
	      )))))))

