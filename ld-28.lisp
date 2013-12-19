;;;; ld-28.lisp

(in-package #:ld-28)

(defvar *screen*)

(defconstant +speed+ 10)

;;; CLASSES
;;; =======

(defclass menu-screen ()
  ((options :accessor options
            :initarg :options)
   (selection :accessor selection
              :initarg :selection))
  (:default-initargs
   :options '("Play" "Quit")
   :selection 0))

(defclass game-screen ()
  ((player :accessor player
           :initarg :player)
   (enemy :accessor enemy
          :initarg :enemy))
  (:default-initargs
   :player (make-instance 'entity
                          :shape '((-25 . -25)
                                   (25 . -25)
                                   (25 . 25)
                                   (-25 . 25))
                          :auto-vectorize t)
    :enemy (make-instance 'entity
                          :shape '((170 . 78) ; notice how this triangle isn't fixed around origin
                                   (220 . 78) ; but it doesn't matter because of auto-vectorize
                                   (195 . 128))
                          :auto-vectorize t)))

(defclass entity ()
  ((shape :accessor shape
          :initarg :shape)
   (transform :accessor transform
              :initarg :transform))
  (:default-initargs
   :transform (lm:make-identity 3)
   :auto-vectorize nil))

;;; If we pass :auto-vectorize t to make-instance 'entity, we can pass
;;; :shape as list of (x . y) conses and it'll convert them to lm:vectors
;;; centered on origin for us
(defmethod initialize-instance :after ((e entity) &key shape auto-vectorize)
  (when auto-vectorize
    (let* ((vertices (mapcar 'cons->vector3 shape))
           (center (shape-center vertices))
           (trans (lm:create-translation-matrix (list (- (lm:x center))
                                                      (- (lm:y center))))))
      (setf (shape e) (mapcar (lambda (v) (lm:* trans v)) vertices)))))

;;; UPDATE METHODS
;;; ==============

(defmethod update ((screen menu-screen))
  )

(defmethod update ((screen game-screen))
  )

;;; DRAW METHODS
;;; ============

(defmethod draw ((screen menu-screen))
  (sdl:clear-display sdl:*black*)
  (with-slots (options selection) screen
    (loop
       :for n :from 0
       :for option :in options
       :do (sdl:draw-string-solid-* option
                                    10
                                    (* 20 (1+ n))
                                    :color (if (= n selection)
                                               sdl:*white*
                                               (sdl:color :r 100 :g 100 :b 100))))))

(defmethod draw ((screen game-screen))
  (sdl:clear-display sdl:*black*)
  ;; border around the game so I can see the edges w/ my messed up windowing...
  (sdl:draw-rectangle-* 0 0 (aref (sdl:video-dimensions) 0) (aref (sdl:video-dimensions) 1)
                        :color (sdl:color :r 90 :g 90 :b 90))
  ;; short in-game instructions
  (sdl:draw-string-solid-* "Player: WASD & Q/W" 10 10 :color (sdl:color :r 90 :g 90 :b 90))
  (sdl:draw-string-solid-* "Enemy: Mouse & Mouse Wheel" 10 20 :color (sdl:color :r 90 :g 90 :b 90))  
  (with-accessors ((p player) (e enemy)) screen
    (draw-shape (mapcar (lambda (v) (lm:* (transform p) v))
                        (shape p)))
    (draw-shape (mapcar (lambda (v) (lm:* (lm:create-translation-matrix (mouse-in-world-coords))
                                          (transform e)
                                          v))
                        (shape e)))))

(defun draw-shape (vertices)
  (let ((dim (sdl:video-dimensions)))
    (sdl:draw-polygon (mapcar (lambda (v)
                                (vector->point
                                 ;; Add half the window dimensions to center origin
                                 (lm:+ v (lm:vector (/ (aref dim 0) 2) (/ (aref dim 1) 2) 0))))
                              vertices))))

;;; HANDLE KEY METHODS
;;; ==================

(defmethod handle-key ((screen menu-screen) key)
  (case key 
    (:sdl-key-escape (sdl:push-quit-event))
    (:sdl-key-up (setf (menu-selection screen)
                       (mod (1- (menu-selection screen))
                            (length (menu-items screen)))))
    (:sdl-key-down (setf (menu-selection screen)
                         (mod (1+ (menu-selection screen))
                              (length (menu-items screen)))))
    (:sdl-key-return (case (menu-selection screen)
                       (0 (setf *screen* (make-instance 'game-screen)))
                       (2 (sdl:push-quit-event))))))

(defmethod handle-key ((screen game-screen) key)
  (with-accessors ((trans transform) (s shape)) (player screen)
    (case key
      (:sdl-key-escape (sdl:push-quit-event))
      (:sdl-key-w (setf trans (lm:* (lm:create-translation-matrix (list 0 (- +speed+))) trans)))
      (:sdl-key-s (setf trans (lm:* (lm:create-translation-matrix (list 0 +speed+)) trans)))
      (:sdl-key-a (setf trans (lm:* (lm:create-translation-matrix (list (- +speed+) 0)) trans)))
      (:sdl-key-d (setf trans (lm:* (lm:create-translation-matrix (list +speed+ 0)) trans)))
      (:sdl-key-e (setf trans (lm:* trans (shape-rotation s +speed+))))
      (:sdl-key-q (setf trans (lm:* trans (shape-rotation s (- +speed+))))))))

;;; HANDLE MOUSE BUTTON DOWN
;;; ========================

(defmethod handle-mouse-button-down ((screen game-screen) button)
  (with-accessors ((trans transform) (s shape)) (enemy screen)
    (cond
      ((= button sdl:sdl-button-wheel-up)
       (setf trans (lm:* trans (shape-rotation s +speed+))))
      ((= button sdl:sdl-button-wheel-down)
       (setf trans (lm:* trans (shape-rotation s (- +speed+))))))))


;;; MAIN LOOP
;;; =========

(defun run (&key (width 460) (height 284)) ; golden ratio yo
  (setf *screen* (make-instance 'game-screen))
  (sdl:with-init ()
    (sdl:window width height :title-caption "You Only Drop Once")
    (setf (sdl:frame-rate) 60)
    (sdl:initialise-default-font sdl:*font-8x8*)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key) (handle-key *screen* key))
      (:mouse-button-down-event (:button button) (handle-mouse-button-down *screen* button))
      (:idle ()
        (update *screen*)     
        (draw *screen*)
        (sdl:update-display)))))

;;; MISC HELPER FUNCTIONS
;;; =====================

(defun cons->vector3 (cons)
  (lm:vector (car cons) (cdr cons) 1))

(defun vector->point (vector)
  (sdl:point :x (lm:x vector) :y (lm:y vector)))

(defun shape-center (vertices)
  (let* ((xs (mapcar 'lm:x vertices))
         (ys (mapcar 'lm:y vertices))
         (center-x (/ (reduce '+ xs) (length xs)))
         (center-y (/ (reduce '+ ys) (length ys))))
    (lm:vector center-x center-y 1)))

;;; gives us a matrix to rotate the given shape around its center
(defun shape-rotation (vertices degrees)
  (let* ((center (shape-center vertices))
         (trans (lm:create-translation-matrix (list (- (lm:x center))
                                                    (- (lm:y center)))))
         (rot (lm:rotation-z 3 (lm:to-radians degrees))))
    (lm:* rot trans)))

(defun mouse-in-world-coords ()
  (let ((x (sdl:mouse-x))
        (y (sdl:mouse-y))
        (w (aref (sdl:video-dimensions) 0))
        (h (aref (sdl:video-dimensions) 1)))
    (list (- x (/ w 2))
          (- y (/ h 2)))))
