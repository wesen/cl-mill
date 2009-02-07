(in-package :cl-user)

(defpackage :gcode
  (:documentation "A package for generating G-Code for NC-machines")
  (:use :cl :uffi :cl-gd :infpre :unit-test)
  (:export))

(in-package :gcode)