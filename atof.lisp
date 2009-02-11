;;; Thu Aug 25 00:56:39 1994 by Mark Kantrowitz <mkant@SKEEZER.OZ.CS.CMU.EDU>
;;; atof.cl -- 7824 bytes

;;; ****************************************************************
;;; PARSE-FLOAT -- equivalent of C's atof **************************
;;; ****************************************************************
;;; 
;;; This program is based loosely on the CMU Common Lisp implementation 
;;; of PARSE-INTEGER.
;;;
;;; ORIGIN: ftp.cs.cmu.edu:/user/ai/lang/lisp/code/math/atof/
;;;
;;; Copyright (c) 1994 by Mark Kantrowitz
;;;
;;; This material was developed by Mark Kantrowitz of the School of
;;; Computer Science, Carnegie Mellon University.
;;;
;;; Permission to use, copy, modify, and distribute this material is
;;; hereby granted, subject to the following terms and conditions.
;;;
;;; In case it be determined by a court of competent jurisdiction that any
;;; provision herein contained is illegal, invalid or unenforceable, such
;;; determination shall solely affect such provision and shall not affect
;;; or impair the remaining provisions of this document.
;;; 
;;; 1. All copies of the software, derivative works or modified versions,
;;;    and any portions thereof, must include this entire copyright and
;;;    permission notice, without modification. The full notice must also
;;;    appear in supporting documentation.
;;; 
;;; 2. Users of this material agree to make their best efforts to inform
;;;    Mark Kantrowitz of noteworthy uses of this material. Correspondence
;;;    should be provided to Mark at:
;;; 
;;;         Mark Kantrowitz
;;;         School of Computer Science
;;;         Carnegie Mellon University
;;;         5000 Forbes Avenue
;;;         Pittsburgh, PA 15213-3891
;;; 
;;;         E-mail: mkant@cs.cmu.edu
;;; 
;;; 3. This software and derivative works may be distributed (but not
;;;    offered for sale) to third parties, provided such third parties
;;;    agree to abide by the terms and conditions of this notice. If you
;;;    modify this software, you must cause the modified file(s) to carry
;;;    a change log describing the changes, who made the changes, and the
;;;    date of the changes.
;;; 
;;; 4. All materials developed as a consequence of the use of this material
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 
;;; 5. Neither the name of Mark Kantrowitz nor any adaptation thereof may
;;;    be used to endorse or promote products derived from this software
;;;    or arising from its use without specific prior written permission
;;;    in each case.
;;; 
;;; 6. Users of this software hereby grant back to Mark Kantrowitz and
;;;    Carnegie Mellon University a non-exclusive, unrestricted, royalty-free
;;;    right and license under any changes, enhancements or extensions made
;;;    to the core functions of the software, including but not limited to
;;;    those affording compatibility with other hardware or software
;;;    environments. Users further agree to use their best efforts to return to
;;;    Mark Kantrowitz any such changes, enhancements or extensions that they
;;;    make.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND MARK KANTROWITZ DISCLAIMS ALL
;;; EXPRESS OR IMPLIED WARRANTIES WITH REGARD TO THIS MATERIAL (INCLUDING
;;; SOFTWARE CONTAINED THEREIN), INCLUDING, WITHOUT LIMITATION, ALL
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE. IN NO EVENT SHALL MARK KANTROWITZ BE LIABLE FOR ANY SPECIAL,
;;; DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
;;; RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
;;; CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE (INCLUDING BUT
;;; NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR
;;; LOSSES SUSTAINED BY THIRD PARTIES OR A FAILURE OF THE PROGRAM TO
;;; OPERATE AS DOCUMENTED). MARK KANTROWITZ IS UNDER NO OBLIGATION TO
;;; PROVIDE ANY SERVICES, BY WAY OF MAINTENANCE, UPDATE, OR OTHERWISE.
;;; 

;;; Change Log:
;;; 26-AUG-94 mk    Suggestions from Richard Lynch: Check for builtin
;;;                 whitespacep before defining (MCL, CMU CL), add #\newline
;;;                 to *whitespace-chars*. 


(in-package :gcode)
; (export '(parse-float))

(eval-when (compile load eval)

  (unless (fboundp 'whitespacep)
    (defparameter *whitespace-chars* 
	'(#\space #\tab #\newline #\return #\linefeed #\page))

    (defun whitespacep (char)
      (find char *whitespace-chars*))))

(defun parse-float (string &key (start 0) end (radix 10) junk-allowed)
  "Converts a substring of STRING, as delimited by START and END, to a 
   floating point number, if possible. START and END default to the 
   beginning and end of the string. RADIX must be between 2 and 36. 
   A floating point number will be returned if the string consists of an
   optional string of spaces and an optional sign, followed by a string
   of digits optionally containing a decimal point, and an optional e or
   E followed by an optionally signed integer. The use of e/E to indicate
   an exponent only works for RADIX = 10. Returns the floating point
   number, if any, and the index for the first character after the number."

  ;; END defaults to the end of the string
  ;; We don't accomplish this by sticking (end (length string)) in the 
  ;; lambda list because I've encountered too many implementations that 
  ;; don't handle such properly. Also, this will work ok if somebody calls
  ;; the function with :end nil.
  (setq end (or end (length string))) 

  ;; Skip over whitespace. If there's nothing but whitespace, signal an error.
  (let ((index (or (position-if-not #'whitespacep string :start start :end end)
                   (if junk-allowed
                       (return-from parse-float (values nil end))
                     (error "No non-whitespace characters in number."))))
        (minusp nil) (decimalp nil) (found-digit nil) 
        (before-decimal 0) (after-decimal 0) (decimal-counter 0)
        (exponent 0)
        (result 0))
    (declare (fixnum index))

    ;; Take care of optional sign.
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (incf index))
            ((char= char #\+)
             (incf index))))

    (loop
     (when (= index end) (return nil))
     (let* ((char (char string index))
            (weight (digit-char-p char radix)))
       (cond ((and weight (not decimalp))
              ;; A digit before the decimal point
              (setq before-decimal (+ weight (* before-decimal radix))
                    found-digit t))
             ((and weight decimalp)
              ;; A digit after the decimal point
              (setq after-decimal (+ weight (* after-decimal radix))
                    found-digit t)
              (incf decimal-counter))
             ((and (char= char #\.) (not decimalp))
	      ;; The decimal point
              (setq decimalp t))
             ((and (char-equal char #\e) (= radix 10))
	      ;; E is for exponent
              (multiple-value-bind (num idx) 
                  (parse-integer string :start (1+ index) :end end
                                 :radix radix :junk-allowed junk-allowed)
                (setq exponent (or num 0)
                      index idx)
		(when (= index end) (return nil))))
             (junk-allowed (return nil))
             ((whitespacep char)
              (when (position-if-not #'whitespacep string
                                     :start (1+ index) :end end)
                (error "There's junk in this string: ~S." string))
              (return nil))
             (t
              (error "There's junk in this string: ~S." string))))
     (incf index))

    ;; Cobble up the resulting number
    (setq result (float (* (+ before-decimal
                              (* after-decimal 
                                 (expt radix (- decimal-counter))))
                           (expt radix exponent))))

    ;; Return the result
    (values
     (if found-digit
         (if minusp (- result) result)
       (if junk-allowed
           nil
         (error "There's no digits in this string: ~S" string)))
     index)))

;;; *EOF*