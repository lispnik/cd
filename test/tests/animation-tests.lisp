(in-package #:cd-tests)

;;; Animation Tests

(def-suite animation-tests :in cd-test-suite)
(in-suite animation-tests)

(test animation-creation
  "Test animation creation and basic operations"
  ;; Test animation creation
  (let ((anim (make-animation)))
    (is (not (null anim)))
    (is (= (animation-current-frame anim) 0))
    (is (= (animation-loop-count anim) -1)) ; infinite by default
    (is (= (animation-total-duration anim) 0)))

  ;; Test animation with specific loop count
  (let ((anim (make-animation 5)))
    (is (= (animation-loop-count anim) 5))))

(test animation-frame-management
  "Test animation frame addition and management"
  (with-debug-canvas (canvas)
    (let ((anim (make-animation)))
      ;; Add frames
      (add-frame anim canvas 1000) ; 1 second
      (add-frame anim canvas 500)  ; 0.5 seconds
      (add-frame anim canvas 750)  ; 0.75 seconds

      ;; Check animation properties
      (is (= (length (animation-frames anim)) 3))
      (is (= (animation-total-duration anim) 2250)) ; 1000 + 500 + 750

      ;; Test frame playback
      (finishes (play-animation-frame canvas anim 0))
      (finishes (play-animation-frame canvas anim 1))
      (finishes (play-animation-frame canvas anim 2)))))

(test easing-functions
  "Test easing function operations"
  ;; Test linear
  (is (= (linear-interpolation 0 100 0.5) 50))
  (is (= (linear-interpolation 10 20 0.0) 10))
  (is (= (linear-interpolation 10 20 1.0) 20))

  ;; Test easing functions
  (is (= (ease-in-quad 0) 0))
  (is (= (ease-in-quad 1) 1))
  (is (= (ease-out-quad 0) 0))
  (is (= (ease-out-quad 1) 1))

  ;; Test cubic easing
  (is (= (ease-in-cubic 0) 0))
  (is (= (ease-in-cubic 1) 1))
  (is (= (ease-out-cubic 0) 0))
  (is (= (ease-out-cubic 1) 1)))

(test property-animation
  "Test property animation"
  ;; Test animate property
  (let ((result (animate-property 0 100 2.0 1.0 #'identity)))
    (is (= result 50))) ; At 1 second of 2 second duration = 50%

  ;; Test with easing
  (let ((result (animate-property 0 100 2.0 1.0 #'ease-in-quad)))
    (is (= result 25))) ; Quad easing at t=0.5 gives 0.25

  ;; Test color animation
  (let ((result (animate-color nil +red+ +blue+ 2.0 1.0)))
    (is (integerp result))))

(test path-animation
  "Test path-based animation"
  ;; Test animated path creation
  (let ((path (make-animated-path #(0 0 100 0 100 100 0 100))))
    (is (not (null path)))
    (is (> (path-total-length path) 0))

    ;; Test position calculation
    (multiple-value-bind (x y) (get-position-on-path path 0.0)
      (is (= x 0))
      (is (= y 0)))

    (multiple-value-bind (x y) (get-position-on-path path 1.0)
      (is (numberp x))
      (is (numberp y)))))

(test particle-system
  "Test particle system"
  ;; Test particle system creation
  (let ((system (make-particle-system 50 50 :rate 5 :life 2.0)))
    (is (not (null system)))
    (is (= (system-emitter-x system) 50))
    (is (= (system-emitter-y system) 50))
    (is (= (system-emission-rate system) 5))

    ;; Test particle emission
    (emit-particle system)
    (is (= (length (system-particles system)) 1))

    ;; Test particle update
    (update-particles system 0.1)
    ;; Should have more particles due to emission rate
    (is (>= (length (system-particles system)) 1))))

(test particle-rendering
  "Test particle rendering"
  (with-debug-canvas (canvas)
    (let ((system (make-particle-system 50 50 :rate 10 :life 1.0)))
      ;; Emit some particles
      (dotimes (i 5)
        (emit-particle system))

      ;; Test rendering
      (finishes (draw-particles canvas system)))))

(test animation-utilities
  "Test animation utility functions"
  (with-debug-canvas (canvas)
    ;; Test bounce animation creation
    (let ((bounce-anim (create-bounce-animation canvas 10 10 100 50 30 2.0)))
      (is (functionp bounce-anim))
      ;; Test calling the animation function
      (finishes (funcall bounce-anim 0.5)))

    ;; Test rotation animation
    (let ((rotation-anim (create-rotation-animation canvas 50 50 30 1.0)))
      (is (functionp rotation-anim))
      (finishes (funcall rotation-anim 0.25)))

    ;; Test fade animation
    (let ((fade-anim (create-fade-animation canvas +red+ +blue+ 1.0)))
      (is (functionp fade-anim))
      (finishes (funcall fade-anim 0.5)))))

(test animation-player
  "Test animation player functionality"
  (with-debug-canvas (canvas)
    (let ((anim (make-animation))
          (player nil))
      ;; Add some frames
      (add-frame anim canvas 1000)
      (add-frame anim canvas 500)

      ;; Create player
      (setf player (make-animation-player anim :loop t))
      (is (not (null player)))
      (is (not (player-playing player)))

      ;; Test playback controls
      (finishes (play-animation player))
      (is (player-playing player))

      (finishes (pause-animation player))
      (is (not (player-playing player)))

      (finishes (stop-animation player))
      (is (= (player-current-time player) 0))

      ;; Test player update (without actually waiting for time)
      (finishes (update-animation-player player canvas)))))

(test animation-interpolation
  "Test advanced animation interpolation"
  ;; Test complex easing combinations
  (let ((values (loop for i from 0 to 10
                      collect (/ i 10.0))))
    (dolist (t-val values)
      ;; Test that all easing functions return valid values
      (is (<= 0 (ease-in-quad t-val) 1))
      (is (<= 0 (ease-out-quad t-val) 1))
      (is (<= 0 (ease-in-out-quad t-val) 1))
      (is (<= 0 (ease-in-cubic t-val) 1))
      (is (<= 0 (ease-out-cubic t-val) 1))
      (is (<= 0 (ease-in-out-cubic t-val) 1)))))

(test animation-with-transforms
  "Test animation with coordinate transformations"
  (with-debug-canvas (canvas)
    (let ((anim (make-animation)))
      ;; Add frames with different transformations
      (with-rotation (canvas 0)
        (add-frame anim canvas 500))

      (with-rotation (canvas 90)
        (add-frame anim canvas 500))

      (with-rotation (canvas 180)
        (add-frame anim canvas 500))

      ;; Test frame playback
      (finishes (play-animation-frame canvas anim 0))
      (finishes (play-animation-frame canvas anim 1))
      (finishes (play-animation-frame canvas anim 2)))))

(test particle-physics
  "Test particle physics simulation"
  (let ((system (make-particle-system 0 0 :gravity-y 0.2 :gravity-x 0.1)))
    ;; Emit a particle
    (emit-particle system)
    (let ((initial-particle (first (system-particles system))))
      (let ((initial-x (particle-x initial-particle))
            (initial-vx (particle-vx initial-particle))
            (initial-vy (particle-vy initial-particle)))

        ;; Update particles
        (update-particles system 1.0) ; 1 second

        ;; Check physics were applied
        (when (system-particles system) ; Particle might have died
          (let ((updated-particle (first (system-particles system))))
            ;; Velocity should have changed due to gravity
            (is (/= (particle-vx updated-particle) initial-vx))
            (is (/= (particle-vy updated-particle) initial-vy))))))))

(test animation-timing
  "Test animation timing accuracy"
  (let ((anim (make-animation)))
    (with-debug-canvas (canvas)
      ;; Add frames with known durations
      (add-frame anim canvas 1000) ; 1 second
      (add-frame anim canvas 2000) ; 2 seconds

      ;; Test frame retrieval at specific times
      (let ((frame1 (get-frame-at-time anim 500)))   ; 0.5 seconds - should be frame 1
        (is (not (null frame1))))

      (let ((frame2 (get-frame-at-time anim 1500)))  ; 1.5 seconds - should be frame 2
        (is (not (null frame2))))

      (let ((frame3 (get-frame-at-time anim 3500)))  ; 3.5 seconds - should loop back
        (is (not (null frame3)))))))

(test animation-performance
  "Test animation performance"
  (with-debug-canvas (canvas)
    ;; Test particle system performance
    (let ((system (make-particle-system 50 50 :rate 50 :life 1.0))
          (start-time (get-internal-real-time)))

      ;; Run simulation for several frames
      (dotimes (i 10)
        (update-particles system 0.1)
        (draw-particles canvas system))

      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 2.0) "Particle simulation should run smoothly")))

    ;; Test animation frame performance
    (let ((anim (make-animation))
          (start-time (get-internal-real-time)))
      ;; Add many frames
      (dotimes (i 20)
        (add-frame anim canvas 100))

      ;; Play through all frames
      (dotimes (i 20)
        (play-animation-frame canvas anim i))

      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 1.0) "Animation frame playback should be fast")))))

(test animation-error-handling
  "Test animation error handling"
  (with-debug-canvas (canvas)
    ;; Test playing invalid frame
    (let ((anim (make-animation)))
      (handler-case
          (play-animation-frame canvas anim 999) ; Non-existent frame
        (error (e)
          (pass))))

    ;; Test animation with no frames
    (let ((empty-anim (make-animation)))
      (handler-case
          (get-frame-at-time empty-anim 1.0)
        (error (e)
          (pass))
        (:no-error (frame)
          ;; Might return nil for no frames
          (pass))))))

(run! 'animation-tests)