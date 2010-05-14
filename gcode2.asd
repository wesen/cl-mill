(defpackage :gcode-system
  (:use :asdf :cl))

(in-package :gcode-system)

(defsystem :gcode2
  :name "GCODE"
  :author "Manuel Odendahl <wesen@ruinwesen.com>"
  :version "0.1"
  :maintainer "Manuel Odendahl <wesen@ruinwesen.com>"
  :serial t
  :depends-on (:unit-test :cl-pdf :cxml :cl-ppcre)
  :components
  (
   ;; thirdparty
   (:file "infpre")
   ;; init
   (:file "package")
   (:file "atof")

   (:file "helpers")
   (:file "init")

   ;; math stuff
   (:file "geometry")
   (:file "arc")
   (:file "bezier")
   (:file "offset")
	 (:file "bbox")

   ;; potrace externals
   #+nil(:file "pot-uffi")

   ;; gcode
   (:file "gcode")
   (:file "opcodes")

	 (:file "tools")

   ;; tracer
   (:file "potrace")

   ;; panelizing and stuff
   (:file "shapes")
   (:file "moves")
   (:file "panel")
   (:file "cube")

   (:file "raster")

   ;; optimizer
   (:file "optimize")

   ;; eagle import
   (:file "drill")

   ;; formats and exporters and importers
   #+nil(:file "sdl")
   (:file "p5")
   (:file "pdf")
   (:file "svg")

   ))