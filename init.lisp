(in-package :gcode)

(defvar *shared-library-directories*
  `( "/usr/local/lib/"
     "/usr/lib/"
     "/usr/lib/cl-gd/"
     "/cygwin/usr/local/lib/"
     "/cygwin/usr/lib/")
  "A list of directories where UFFI tries to find libpotrace")
(defvar *shared-library-types* '("so" "dll" "dylib")
  "The list of types a shared library can have. Used when looking for
libpotrace")
(defvar *shared-library-drive-letters* '("C" "D" "E" "F" "G")
  "The list of drive letters \(used by Wintendo) used when looking for
libpotrace")

(defun load-potrace (&key force-load)
  (let ((filename (find-foreign-library "pot-glue"
                                        *shared-library-directories*
                                        :types *shared-library-types*
                                        :drive-letters *shared-library-drive-letters*)))
    (load-foreign-library filename
                          :module "potrace"
			  :force-load force-load
			  :supporting-libraries '("potrace"))))

(load-potrace)