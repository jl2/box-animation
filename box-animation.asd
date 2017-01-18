;;;; box-animation.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:box-animation
  :description "Describe box-animation here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qt
               #:qtools
               #:qtgui
               #:qtcore
               #:anim-utils
               #:mixalot-mp3
               #:qtopengl
               #:cl-opengl
               #:cl-glu
               #:cl-glut
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "box-animation")))

