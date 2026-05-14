(in-package #:cd)

;;; Animation Support

(defclass animation-frame ()
  ((canvas-state :initarg :canvas-state :accessor frame-canvas-state)
   (duration :initarg :duration :accessor frame-duration)
   (delay :initarg :delay :accessor frame-delay :initform 0)
   (dispose-method :initarg :dispose-method :accessor frame-dispose-method :initform :none))
  (:documentation "A single animation frame"))

(defclass animation ()
  ((frames :initform '() :accessor animation-frames)
   (current-frame :initform 0 :accessor animation-current-frame)
   (loop-count :initform -1 :accessor animation-loop-count) ; -1 = infinite
   (total-duration :initform 0 :accessor animation-total-duration))
  (:documentation "Animation sequence"))

(defun make-animation (&optional (loop-count -1))
  "Create a new animation"
  (make-instance 'animation :loop-count loop-count))

(defun add-frame (animation canvas duration &optional (delay 0) (dispose-method :none))
  "Add a frame to animation by capturing current canvas state"
  (validate-canvas canvas)
  (let ((state (save-state canvas)))
    (push (make-instance 'animation-frame
                         :canvas-state state
                         :duration duration
                         :delay delay
                         :dispose-method dispose-method)
          (animation-frames animation))
    (incf (animation-total-duration animation) duration)))

(defun get-frame-at-time (animation time)
  "Get the frame that should be displayed at given time"
  (let ((current-time 0)
        (frames (reverse (animation-frames animation))))
    (dolist (frame frames)
      (when (<= current-time time (+ current-time (frame-duration frame)))
        (return-from get-frame-at-time frame))
      (incf current-time (frame-duration frame)))
    ;; If we get here, handle looping
    (when (> (animation-loop-count animation) 0)
      (let ((loop-time (mod time (animation-total-duration animation))))
        (get-frame-at-time animation loop-time)))))

(defun play-animation-frame (canvas animation frame-index)
  "Play a specific frame of animation"
  (validate-canvas canvas)
  (let ((frame (nth frame-index (reverse (animation-frames animation)))))
    (when frame
      (restore-state canvas (frame-canvas-state frame))
      (setf (animation-current-frame animation) frame-index))))

(defun animate-canvas (canvas animation start-time current-time)
  "Update canvas to show animation at current time"
  (validate-canvas canvas)
  (let ((elapsed (- current-time start-time)))
    (when (>= elapsed 0)
      (let ((frame (get-frame-at-time animation elapsed)))
        (when frame
          (restore-state canvas (frame-canvas-state frame)))))))

;;; Interpolation and Tweening

(deftype easing-function ()
  "Easing function type"
  '(function (real) real))

(defun linear-interpolation (start end t-val)
  "Linear interpolation between start and end"
  (declare (type real start end t-val))
  (+ start (* t-val (- end start))))

(defun ease-in-quad (t-val)
  "Quadratic ease-in"
  (* t-val t-val))

(defun ease-out-quad (t-val)
  "Quadratic ease-out"
  (- (* t-val (- t-val 2))))

(defun ease-in-out-quad (t-val)
  "Quadratic ease-in-out"
  (if (< t-val 0.5)
      (* 2 t-val t-val)
      (1- (* -2 t-val t-val (- 2 t-val)))))

(defun ease-in-cubic (t-val)
  "Cubic ease-in"
  (* t-val t-val t-val))

(defun ease-out-cubic (t-val)
  "Cubic ease-out"
  (let ((t1 (1- t-val)))
    (1+ (* t1 t1 t1))))

(defun ease-in-out-cubic (t-val)
  "Cubic ease-in-out"
  (if (< t-val 0.5)
      (* 4 t-val t-val t-val)
      (1+ (* 4 (1- t-val) (1- t-val) (1- t-val)))))

(defun animate-property (start-value end-value duration current-time easing-fn)
  "Animate a property value using easing"
  (declare (type easing-function easing-fn))
  (let ((t-val (max 0 (min 1 (/ current-time duration)))))
    (linear-interpolation start-value end-value (funcall easing-fn t-val))))

(defclass property-animation ()
  ((property-getter :initarg :getter :accessor animation-property-getter)
   (property-setter :initarg :setter :accessor animation-property-setter)
   (start-value :initarg :start-value :accessor animation-start-value)
   (end-value :initarg :end-value :accessor animation-end-value)
   (duration :initarg :duration :accessor animation-duration)
   (easing :initarg :easing :accessor animation-easing :initform #'identity)
   (start-time :initarg :start-time :accessor animation-start-time :initform 0))
  (:documentation "Property animation definition"))

(defun animate-color (canvas start-color end-color duration current-time &optional (easing #'identity))
  "Animate color transition"
  (multiple-value-bind (r1 g1 b1) (decode-color start-color)
    (multiple-value-bind (r2 g2 b2) (decode-color end-color)
      (let ((t-val (max 0 (min 1 (/ current-time duration)))))
        (let ((eased-t (funcall easing t-val)))
          (encode-color (round (linear-interpolation r1 r2 eased-t))
                        (round (linear-interpolation g1 g2 eased-t))
                        (round (linear-interpolation b1 b2 eased-t))))))))

;;; Path Animation

(defclass animated-path ()
  ((control-points :initarg :control-points :accessor path-control-points)
   (total-length :initform 0 :accessor path-total-length)
   (segments :initform '() :accessor path-segments))
  (:documentation "Path for animation following"))

(defun make-animated-path (points)
  "Create an animated path from control points"
  (let ((path (make-instance 'animated-path :control-points points)))
    (calculate-path-segments path)
    path))

(defun calculate-path-segments (path)
  "Calculate path segments and total length"
  (let ((points (path-control-points path))
        (segments '())
        (total-length 0))
    (loop for i from 0 below (1- (length points)) by 2
          for x1 = (aref points i)
          for y1 = (aref points (1+ i))
          for x2 = (aref points (+ i 2))
          for y2 = (aref points (+ i 3))
          for length = (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))
          do (push (list x1 y1 x2 y2 length total-length) segments)
             (incf total-length length))
    (setf (path-segments path) (nreverse segments))
    (setf (path-total-length path) total-length)))

(defun get-position-on-path (path t-val)
  "Get position on path at parameter t (0 to 1)"
  (let ((target-distance (* t-val (path-total-length path)))
        (current-distance 0))
    (dolist (segment (path-segments path))
      (destructuring-bind (x1 y1 x2 y2 length start-distance) segment
        (when (<= target-distance (+ current-distance length))
          (let ((segment-t (if (> length 0)
                               (/ (- target-distance current-distance) length)
                               0)))
            (return-from get-position-on-path
              (values (+ x1 (* segment-t (- x2 x1)))
                      (+ y1 (* segment-t (- y2 y1))))))
        (incf current-distance length)))
    ;; Return last point if we somehow don't find it
    (let ((points (path-control-points path)))
      (values (aref points (- (length points) 2))
              (aref points (- (length points) 1))))))

(defun animate-along-path (canvas path object-drawer duration current-time)
  "Animate object along path"
  (validate-canvas canvas)
  (let ((t-val (max 0 (min 1 (/ current-time duration)))))
    (multiple-value-bind (x y) (get-position-on-path path t-val)
      (funcall object-drawer canvas (round x) (round y)))))

;;; Particle System

(defclass particle ()
  ((x :initarg :x :accessor particle-x)
   (y :initarg :y :accessor particle-y)
   (vx :initarg :vx :accessor particle-vx :initform 0)
   (vy :initarg :vy :accessor particle-vy :initform 0)
   (life :initarg :life :accessor particle-life :initform 1.0)
   (max-life :initarg :max-life :accessor particle-max-life :initform 1.0)
   (size :initarg :size :accessor particle-size :initform 2)
   (color :initarg :color :accessor particle-color :initform +white+))
  (:documentation "A single particle"))

(defclass particle-system ()
  ((particles :initform '() :accessor system-particles)
   (emitter-x :initarg :x :accessor system-emitter-x)
   (emitter-y :initarg :y :accessor system-emitter-y)
   (emission-rate :initarg :rate :accessor system-emission-rate :initform 10)
   (particle-life :initarg :life :accessor system-particle-life :initform 2.0)
   (gravity-x :initarg :gravity-x :accessor system-gravity-x :initform 0)
   (gravity-y :initarg :gravity-y :accessor system-gravity-y :initform 0.1))
  (:documentation "Particle system"))

(defun make-particle-system (x y &key (rate 10) (life 2.0) (gravity-x 0) (gravity-y 0.1))
  "Create a new particle system"
  (make-instance 'particle-system
                 :x x :y y
                 :rate rate :life life
                 :gravity-x gravity-x :gravity-y gravity-y))

(defun emit-particle (system)
  "Emit a new particle from the system"
  (let ((angle (* (random 360) (/ pi 180)))
        (speed (+ 1 (random 3.0))))
    (push (make-instance 'particle
                         :x (system-emitter-x system)
                         :y (system-emitter-y system)
                         :vx (* speed (cos angle))
                         :vy (* speed (sin angle))
                         :life (system-particle-life system)
                         :max-life (system-particle-life system)
                         :size (+ 1 (random 3))
                         :color (encode-color (+ 128 (random 128))
                                              (+ 128 (random 128))
                                              (+ 128 (random 128))))
          (system-particles system))))

(defun update-particles (system dt)
  "Update all particles in the system"
  (setf (system-particles system)
        (remove-if (lambda (particle)
                     ;; Update particle position and velocity
                     (incf (particle-x particle) (* (particle-vx particle) dt))
                     (incf (particle-y particle) (* (particle-vy particle) dt))
                     (incf (particle-vx particle) (* (system-gravity-x system) dt))
                     (incf (particle-vy particle) (* (system-gravity-y system) dt))
                     (decf (particle-life particle) dt)
                     ;; Remove dead particles
                     (<= (particle-life particle) 0))
                   (system-particles system)))

  ;; Emit new particles
  (dotimes (i (round (* (system-emission-rate system) dt)))
    (emit-particle system)))

(defun draw-particles (canvas system)
  "Draw all particles in the system"
  (validate-canvas canvas)
  (let ((saved-foreground (foreground canvas)))
    (unwind-protect
         (dolist (particle (system-particles system))
           (let ((alpha (/ (particle-life particle) (particle-max-life particle))))
             ;; Fade particle based on remaining life
             (setf (foreground canvas)
                   (encode-color-alpha
                    (red (particle-color particle))
                    (green (particle-color particle))
                    (blue (particle-color particle))
                    (round (* alpha 255))))
             (let ((size (particle-size particle)))
               (sector canvas
                       (round (particle-x particle))
                       (round (particle-y particle))
                       size size 0 360))))
      (setf (foreground canvas) saved-foreground))))

;;; Animation Utilities

(defmacro with-animation-frame ((canvas) &body body)
  "Execute body as an animation frame"
  `(progn
     (clear ,canvas)
     ,@body
     (flush ,canvas)))

(defun create-bounce-animation (canvas x y width height bounce-height duration)
  "Create a bouncing ball animation"
  (validate-canvas canvas)
  (lambda (time)
    (let* ((t-val (mod (/ time duration) 1.0))
           (bounce-y (+ y (* bounce-height (abs (sin (* t-val pi))))))
           (ball-x (+ x (* t-val width))))
      (with-animation-frame (canvas)
        (setf (foreground canvas) +red+)
        (sector canvas (round ball-x) (round bounce-y) 20 20 0 360)))))

(defun create-rotation-animation (canvas cx cy radius duration)
  "Create a rotating object animation"
  (validate-canvas canvas)
  (lambda (time)
    (let* ((angle (* (/ time duration) 360))
           (x (+ cx (* radius (cos (* angle (/ pi 180))))))
           (y (+ cy (* radius (sin (* angle (/ pi 180)))))))
      (with-animation-frame (canvas)
        (setf (foreground canvas) +blue+)
        (sector canvas (round x) (round y) 10 10 0 360)))))

(defun create-fade-animation (canvas start-color end-color duration)
  "Create a color fade animation"
  (validate-canvas canvas)
  (lambda (time)
    (let ((current-color (animate-color canvas start-color end-color duration time)))
      (setf (background canvas) current-color)
      (clear canvas))))

;;; Animation Playback

(defclass animation-player ()
  ((animation :initarg :animation :accessor player-animation)
   (start-time :initform 0 :accessor player-start-time)
   (current-time :initform 0 :accessor player-current-time)
   (playing :initform nil :accessor player-playing)
   (loop :initform t :accessor player-loop))
  (:documentation "Animation playback controller"))

(defun make-animation-player (animation &key (loop t))
  "Create an animation player"
  (make-instance 'animation-player :animation animation :loop loop))

(defun play-animation (player)
  "Start playing animation"
  (setf (player-playing player) t)
  (setf (player-start-time player) (get-internal-real-time)))

(defun pause-animation (player)
  "Pause animation"
  (setf (player-playing player) nil))

(defun stop-animation (player)
  "Stop animation and reset"
  (setf (player-playing player) nil)
  (setf (player-current-time player) 0))

(defun update-animation-player (player canvas)
  "Update animation player and render current frame"
  (when (player-playing player)
    (let ((current-real-time (get-internal-real-time)))
      (setf (player-current-time player)
            (/ (- current-real-time (player-start-time player))
               internal-time-units-per-second))

      ;; Handle looping
      (when (and (player-loop player)
                 (> (player-current-time player) (animation-total-duration (player-animation player))))
        (setf (player-start-time player) current-real-time)
        (setf (player-current-time player) 0))

      ;; Render current frame
      (animate-canvas canvas (player-animation player) 0 (player-current-time player)))))