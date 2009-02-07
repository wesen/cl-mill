(defpackage :gcode-system
  (:use :asdf :cl))

(in-package :gcode-system)

(defsystem :gcode
  :name "GCODE"
  :author "Manuel Odendahl <wesen@ruinwesen.com>"
  :version "0.1"
  :maintainer "Manuel Odendahl <wesen@ruinwesen.com>"
  :serial t
  :depends-on (:cl-gd :uffi :cocoahelper :lispbuilder-sdl :unit-test :cl-pdf)
  :components
  (
   (:file "infpre")

   (:file "package")
   (:file "helpers")
   (:file "init")

   (:file "geometry")
   (:file "arc")
   (:file "bezier")
   (:file "offset")

   (:file "pot-uffi")
   (:file "gcode")
   (:file "opcodes")

   (:file "potrace")

   (:file "shapes")
   (:file "moves")
   (:file "panel")
   (:file "cube")

   (:file "raster")


   (:file "optimize")
   (:file "drill")

   (:file "sdl")

   (:file "p5")

   (:file "pdf")

   ))