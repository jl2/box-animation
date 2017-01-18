;;;; box-animation.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:box-animation)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defparameter *fps* 60)
(defparameter *fft-window-size* 1024)
(defparameter *rotation-step* 4.0)

(defstruct box-animation
  "The parameters used for drawing a box-animation image."
  (steps 8 :type (unsigned-byte 32))
  (rotation-scale 10.0 :type double-float)
  (color-scale 2.0 :type double-float))

(define-widget box-animation-animator (QGLWidget)
  ((the-mp3 :initform nil)
   (start-time :initform 0)
   (song-duration :initform 0)
   (window-buffer :initform (make-array *fft-window-size* 
                                        :element-type '(complex double-float)
                                        :adjustable nil
                                        :fill-pointer nil))
   (left-fft-data :initform (make-array *fft-window-size* 
                                        :element-type '(complex double-float)
                                        :adjustable nil
                                        :fill-pointer nil))
   (right-fft-data :initform (make-array *fft-window-size* 
                                         :element-type '(complex double-float)
                                         :adjustable nil
                                         :fill-pointer nil))
   
   (box-anim :initform (make-box-animation)))
  (:documentation "The box-animation-animator widget draws animated cubes."))

(define-subwidget (box-animation-animator timer) (q+:make-qtimer box-animation-animator)
  (setf (q+:single-shot timer) nil))

(define-initializer (box-animation-animator setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background box-animation-animator) nil)
  (setf (q+:auto-buffer-swap box-animation-animator) nil))

(define-slot (box-animation-animator tick) ()
  (declare (connected timer (timeout)))
  (q+:repaint box-animation-animator))

(define-override (box-animation-animator initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (box-animation-animator resize-g-l) (width height)
  )

(defun max-radius (box-anim)
  (declare (ignorable box-anim))
  10.0)

(defun cube-square (steps radius red-line green-line blue-line
                    red-fill green-fill blue-fill)
  (let ((ds (/ (* 2 radius) steps)))
    (loop for i to steps
       do
         (let ((x-loc (- (* i ds) radius)))

           ;; Top
           (gl:push-matrix)
           (gl:translate x-loc radius 0.0)
           (gl:color red-fill green-fill blue-fill)
           (cl-glut:solid-cube ds)
           (gl:color red-line green-line blue-line)
           (cl-glut:wire-cube ds)
           (gl:pop-matrix)

           ;; Bottom
           (gl:push-matrix)
           (gl:translate x-loc (- radius) 0.0)
           (gl:color red-fill green-fill blue-fill)
           (cl-glut:solid-cube ds)
           (gl:color red-line green-line blue-line)
           (cl-glut:wire-cube ds)
           (gl:pop-matrix)
           
           (when (and (/= i 0) (/= i steps ))

             ;; Left
             (gl:push-matrix)
             (gl:translate (- radius) x-loc 0.0)
             (gl:color red-fill green-fill blue-fill)
             (cl-glut:solid-cube ds)
             (gl:color red-line green-line blue-line)
             (cl-glut:wire-cube ds)
             (gl:pop-matrix)

             ;; Right
             (gl:push-matrix)
             (gl:translate radius x-loc 0.0)
             (gl:color red-fill green-fill blue-fill)
             (cl-glut:solid-cube ds)
             (gl:color red-line green-line blue-line)
             (cl-glut:wire-cube ds)
             (gl:pop-matrix))))))

(define-override (box-animation-animator paint-g-l paint) ()
  "Handle paint events."
  (with-slots (steps rotation-scale color-scale) box-anim
    (let* ((max-radius (max-radius box-anim))
           (width (q+:width box-animation-animator))
           (height (q+:height box-animation-animator))
           (x-aspect-ratio (if (< height width)
                               (/ height width 1.0d0)
                               1.0d0))
           (y-aspect-ratio (if (< height width)
                               1.0d0
                               (/ width height 1.0d0)))
           
           (location (/ (+ 1 (- (get-internal-real-time) start-time)) 1.0 internal-time-units-per-second))
           (win-center (ceiling (max 0 
                                     (- (* 44100 location)
                                        (round (/ *fft-window-size* 2)))))))

      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter box-animation-animator)))

        (q+:begin-native-painting painter)

        ;; (gl:viewport 0 0 width height)
        ;; (gl:matrix-mode :projection)
        ;; (gl:load-identity)
        ;; (gl:ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)

        (gl:viewport 0 0 width height)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (glu:perspective 50 (/ height width) 1.0 5000.0)
        (glu:look-at 0 0 14
                     0 0 0
                     0 1 0)

        (gl:clear-color 0 0 0 1)
        (gl:enable :line-smooth :polygon-smooth
                   :depth-test :depth-clamp :alpha-test
                   :polygon-offset-ext)

        (gl:matrix-mode :modelview)
        (gl:load-identity)

        (gl:clear :color-buffer :depth-buffer)
        ;; Clear the background
        (when (and the-mp3 (< (/ (- (get-internal-real-time) start-time) 1.0 internal-time-units-per-second) song-duration))
          (bordeaux-fft:windowed-fft! (mp3-file-left-channel the-mp3)
                                      window-buffer left-fft-data
                                      win-center *fft-window-size* 'bordeaux-fft:triangle)
          (bordeaux-fft:windowed-fft! (mp3-file-right-channel the-mp3) 
                                      window-buffer right-fft-data
                                      win-center *fft-window-size* 'bordeaux-fft:triangle))
        
        ;; Actual drawing goes here.  In this case, just a line.
        (gl:push-matrix)


        ;; TODO: Use "modern" OpenGL
        ;;        (gl:rotate rotation 0 0 1.0)
        (gl:polygon-mode :front-and-back :fill)
        (gl:polygon-offset 1.0 1.4)

        (gl:scale x-aspect-ratio y-aspect-ratio 1.0)
        (loop for i below (* 10 steps)
           do
             (let ((r-amount (* rotation-scale (/ (abs (aref right-fft-data i))  (/ *fft-window-size* 2)))))
               (if (> (abs (aref right-fft-data i)) (abs (aref left-fft-data i)))
                   (gl:rotate r-amount 0 0 1.0)
                   (gl:rotate (- r-amount) 0 0 1.0)))
             (gl:translate 0.0 (/ (abs (aref right-fft-data i)) (/ *fft-window-size* 2)) (- (/ 12.0 steps)))
             (cube-square steps
                          6.0
                          1.0 0.0 0.0 
                          0.0 (* color-scale (/ (abs (aref left-fft-data i)) (/ *fft-window-size* 2))) 0.0))

        (gl:pop-matrix)
        
        
        (q+:swap-buffers box-animation-animator)
        (q+:end-native-painting painter)))))

(define-widget box-animation-widget (QWidget)
  ()
  (:documentation "A Box-Animation animator and its controls."))

(define-subwidget (box-animation-widget fft-viewer) (make-instance 'box-animation-animator)
  "The box-animation-drawer itself.")


(define-subwidget (box-animation-widget mp3-file-edit) (q+:make-qlineedit box-animation-widget)
  "The currently open file."
  (setf (q+:read-only mp3-file-edit) t))

(define-subwidget (box-animation-widget steps-spin) (q+:make-qspinbox box-animation-widget)
  "The spinbox for the number of steps."
  (q+:set-maximum steps-spin 10000000)
  (q+:set-minimum steps-spin 4)
  (q+:set-value steps-spin (box-animation-steps (slot-value fft-viewer 'box-anim))))

(define-slot (box-animation-widget steps-changed) ((value int))
  "Handle changes to the steps-spin box."
  (declare (connected steps-spin (value-changed int)))
  (setf (box-animation-steps (slot-value fft-viewer 'box-anim)) (q+:value steps-spin))
  (q+:repaint fft-viewer))

(define-subwidget (box-animation-widget rotation-spin) (q+:make-qdoublespinbox box-animation-widget)
  "The spinbox for the rotation scale factor."
  (q+:set-decimals rotation-spin 2)
  (q+:set-single-step rotation-spin 0.25)
  (q+:set-maximum rotation-spin 100000.0)
  (q+:set-minimum rotation-spin 0.25)
  (q+:set-value rotation-spin (box-animation-rotation-scale (slot-value fft-viewer 'box-anim))))

(define-subwidget (box-animation-widget color-spin) (q+:make-qdoublespinbox box-animation-widget)
  "The spinbox for the color scale factor."
  (q+:set-decimals color-spin 2)
  (q+:set-single-step color-spin 0.25)
  (q+:set-maximum color-spin 100000.0)
  (q+:set-minimum color-spin 0.25)
  (q+:set-value color-spin (box-animation-color-scale (slot-value fft-viewer 'box-anim))))

(define-slot (box-animation-widget values-changed) ((value double))
  "Handle changes to all of the spin boxes except steps."
  (declare (connected rotation-spin (value-changed double)))
  (declare (connected color-spin (value-changed double)))
  
  (with-slots (rotation-scale color-scale) (slot-value fft-viewer 'box-anim)
    (setf rotation-scale (q+:value rotation-spin))
    (setf color-scale (q+:value color-spin))
    (q+:repaint fft-viewer)))

(define-subwidget (box-animation-widget control-layout) (q+:make-qvboxlayout box-animation-widget)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let ((file-layout (q+:make-qhboxlayout))
        (other-layout (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout

    (q+:add-widget file-layout (q+:make-qlabel "Filename: " box-animation-widget))
    (q+:add-widget file-layout mp3-file-edit)


    (q+:add-widget other-layout (q+:make-qlabel "Rotation scale: " box-animation-widget))
    (q+:add-widget other-layout rotation-spin)

    (q+:add-widget other-layout (q+:make-qlabel "Color scale: " box-animation-widget))
    (q+:add-widget other-layout color-spin)

    (q+:add-widget other-layout (q+:make-qlabel "steps: " box-animation-widget))
    (q+:add-widget other-layout steps-spin)

    (q+:add-layout control-layout file-layout)
    (q+:add-layout control-layout other-layout)

    ;; Finally add the box-animation viewer directly to the vertical layout
    (q+:add-widget control-layout fft-viewer)))


(define-signal (box-animation-widget open-mp3) (string))

(define-slot (box-animation-widget open-mp3) ((file-name string))
  (declare (connected box-animation-widget (open-mp3 string)))
  (let* ((new-mp3-file (read-mp3-file file-name))
         (sduration (mp3-file-duration-in-seconds new-mp3-file))
         (tframes (ceiling (* sduration *fps*))))

    (setf (q+:text mp3-file-edit) file-name)
    (setf (slot-value fft-viewer 'the-mp3) new-mp3-file)
    (setf (slot-value fft-viewer 'song-duration) sduration)
    (setf (slot-value fft-viewer 'start-time) (get-internal-real-time))))



(define-widget main-window (QMainWindow)
  ((mixer :initform (mixalot:create-mixer))
   (current-stream :initform nil)))

(define-override (main-window close-event) (ev)
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Open MP3" (ctrl o))
         (open-mp3 main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Cube animation synced to an MP3 file using the FFT.")))

(define-subwidget (main-window box-animation-viewer) (make-instance 'box-animation-widget)
  "The central box-animation-widget.")


(define-slot (main-window open open-mp3) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.music-location))
                                                     "*.mp3")))
    (when (and filename (> (length filename) 0))
      (signal! box-animation-viewer (open-mp3 string) filename)
      (when current-stream (mixalot:mixer-remove-streamer mixer current-stream))
      (setf current-stream (mixalot-mp3:make-mp3-streamer filename))
      (mixalot:mixer-add-streamer mixer current-stream))))

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (cl-glut:init)
  (setf (q+:central-widget main-window) box-animation-viewer))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
