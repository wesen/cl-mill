(in-package :gcode)

(defparameter *pcb-tool*
  (make-instance 'tool
		 :diameter 1
		 :number 7
		 :feed-xy 600
		 :feed-z 240
		 :depth 2.2))

(defparameter *engrave-tool*
  (make-instance 'tool
		 :diameter 1
		 :number 9
		 :feed-xy 600
		 :feed-z 240
		 :depth 1.5))


(defun pcb (file outfile)
  (program-to-file
   (with-program ("pcb")
     (with-tool (*pcb-tool*)
       (tool-up)
       (home)
       (load-file
	file)))
   outfile :order '("drills" "mill" "bridge-cut")))

#+nil
(defun test-file ()
  (with-program ("file-tool")
    (with-tool (*pcb-tool*)
      (tool-up)
      (home)
      (load-file
       "/Users/manuel/siff-svn/ruinwesen/eagle/midicommand/minicommand-ioboard.lisp"))))

(defvar *drills*)

(defvar *eagle-drills-p* t)
(defvar *eagle-vias-p* nil)

(defun add-drill (&key x y diameter type)
  (when *eagle-drills-p*
    (push (list x y diameter type) *drills*)))

(defun add-via (&key x y diameter)
  (when *eagle-vias-p*
    (push (list x y diameter) *drills*)))


(defun square (x)
  (* x x))

(defun distance (x1 y1 x2 y2)
  (sqrt (+
	 (square (- x1 x2))
	 (square (- y1 y2)))))

(defun nearest-drill (x y drills)
  (let ((distances
	 (sort 
	  (mapcar #'(lambda (drill)
		      (cons (distance x y (first drill) (second drill))
			    (copy-tree drill)))
		  drills)
	  #'< :key #'first)))
    (cdr (first distances))))

(defun drill-direction (x1 y1 x2 y2)
  (let ((direction nil))
    (cond ((> x2 x1) (push :right direction))
	  ((< x2 x1) (push :left direction)))
    (cond ((> y2 y1) (push :up direction))
	  ((< y2 y1) (push :down direction)))
    direction))

(defun sort-drills ()
  *drills*)

(defmacro with-drills (() &rest body)
  `(let ((*drills*))
     ,@body
     (let ((*drills* (sort-drills)))
       (with-named-pass ("drills")
	 (with-tool (*pcb-tool*)
	   (dolist (drill *drills*)
	     (drill :x (first drill)
		    :y (second drill) ;; XXXX invert!!!
		    :diameter (third drill))))))))



(defun optimize-test-file ()
  (let ((program (test-file)))
    (optimize-program-pass program "drills")
    program))