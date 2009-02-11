(defpackage :gcode-system
  (:use :asdf :cl))

(in-package :gcode-system)

(defsystem :gcode
  :name "GCODE"
  :author "Manuel Odendahl <wesen@ruinwesen.com>"
  :version "0.1"
  :maintainer "Manuel Odendahl <wesen@ruinwesen.com>"
  :serial t
  :depends-on (:cl-gd :uffi :cocoahelper :lispbuilder-sdl :unit-test :cl-pdf :cxml)
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

   ;; potrace externals
   (:file "pot-uffi")

   ;; gcode
   (:file "gcode")
   (:file "opcodes")

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
   (:file "sdl")
   (:file "p5")
   (:file "pdf")
   (:file "svg")

   ))