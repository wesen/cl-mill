(in-package :gcode)

;; g-code list representation (very hackish)

(defun g-to-file (gcode file &key (debug nil))
  (with-open-file (s file :if-exists :supersede :if-does-not-exist :create
		     :direction :output)
    (g-to-stream gcode s))
  (when debug
    (with-open-file (s file :direction :input)
      (loop for line = (read-line s nil :eof)
	   until (eql line :eof)
	   do (princ line)
	   (princ #\newline)))))

(defun g-code-listp (gcode)
  (and (listp gcode)
       (listp (first gcode))))

(defun g-code-line-p (gcode)
  (and (listp gcode)
       (keywordp (first gcode))))

(defun g-to-stream (gcode &optional (s *standard-output*) (cnt 10))
  (loop for line in gcode
     do (if (g-code-listp line)
	    ;; nested program
	    (setf cnt (g-to-stream line s cnt))
	    (progn (format s "N~A ~A~%" cnt (g-line line))
		   (incf cnt 10))))
  cnt)

#+nil
(defmacro g-program (&rest code)
  (let ((i (gensym))
	(res (gensym)))
    `(let ((,i (list)))
       ,@(loop for part in code
	    collect (if (g-code-line-p part)
			`(push ',part ,i)
			`(let ((,res ,part))
			   (when (or (listp ,res) (g-code-line-p ,res))
			     (push ,res ,i)))))
       (nreverse (remove nil ,i)))))


(defun g-param (code param)
  (second (assoc param (cdr code))))

(defun g-line (line)
  (with-output-to-string (s)
    (format s "~{~A~^ ~}" (mapcar #'g-elt line))))

(defun g-elt (elt)
  (cond ((listp elt)
	 (format nil "~A~{~A~^ ~}"
		 (first elt)
		 (mapcar #'realize-number (cdr elt))))
	(t (princ-to-string elt))))

(defun load-file (file)
  (with-open-file (s file :direction :input)
    (eval (cons 'progn (loop for line = (read s nil :eof)
;;			    do (format t "line: ~S~%" line)
			  until (eql line :eof)
			  collect line)))))
  
