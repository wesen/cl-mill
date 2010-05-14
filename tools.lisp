(in-package :gcode)


(defparameter *plywood-board-tool2*
  (make-instance 'tool
								 :diameter 1
								 :number 1
								 :feed-xy 600
								 :feed-z 240
								 :depth 4))

(defparameter *alu-tool-top*
  ;; FUER FRONTPLATTEN
  (make-instance 'tool :diameter 2 :depth 1.3 :number 8 :feed-xy 500 :feed-z 100))

(defparameter *alu-tool*
  (make-instance 'tool :diameter 2 :depth 0.5 :number 8 :feed-xy 500 :feed-z 100))

(defparameter *schraeg-tool*
	(make-instance 'tool :diameter 0 :depth 1 :number 18))

(defparameter *paper-tool*
  (make-instance 'tool :diameter 0 :depth 2.6 :number 8 :feed-xy 500 :feed-z 100))


(defparameter *pcb-gravier-tool*
  (make-instance 'tool :diameter 0.5 :number 22))

(defparameter *grob-tool*
  (make-instance 'tool
								 :number 17
								 :diameter 3
								 :depth 2))

(defparameter *mdf-tool-2mm*
  (make-instance 'tool
		 :diameter 2
		 :number 10
		 :depth 3))
		 

(defparameter *cube-tool*
  (make-instance 'tool
								 :diameter 1
								 :feed-xy 600
								 :feed-z 240
								 :depth 4.0))

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


(defparameter *kiefer-tool*
  (make-instance 'tool
		 :diameter 2
		 :number 11
		 :feed-xy 600
		 :feed-z 240
		 :depth 1.5))

(defparameter *plywood-board-tool*
  (make-instance 'tool
		 :diameter 1
		 :number 6
		 :feed-xy 600
		 :feed-z 240
		 :depth 2))


(defparameter *trace-tool*
  *plywood-board-tool*)


(defparameter *dick-holz-tool*
  (make-instance 'tool
		 :number 16
		 :diameter 3
		 :depth 3))

