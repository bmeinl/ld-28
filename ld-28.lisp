;;;; ld-28.lisp

(in-package #:ld-28)

;;; Only global thus far!
(defvar *screen*)

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
           :initarg :player))
  (:default-initargs
   :player (make-instance 'entity
                          :shape '((-25 . -25)
                                   (25 . -25)
                                   (25 . 25)
                                   (-25 . 25))
                          :auto-vectorize t)))

(defclass entity ()
  ((shape :accessor shape
          :initarg :shape)
   (transform :accessor transform
              :initarg :transform))
  (:default-initargs
   :transform (lm:make-identity 3)
   :auto-vectorize nil))

(defmethod initialize-instance :after ((e entity) &key shape auto-vectorize)
  (when auto-vectorize
    (setf (shape e) (mapcar 'cons->vector3 shape))))

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
  )

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
  (case key
    (:sdl-key-escape (sdl:push-quit-event))
    (:sdl-key-left (setf (game-level screen) (max 0 (1- (game-level screen)))))
    (:sdl-key-right (incf (game-level screen)))))

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
      (:idle ()
        (update *screen*)     
        (draw *screen*)
        (sdl:update-display)))))

;;; MISC HELPER FUNCTIONS
;;; =====================

(defun cons->vector3 (cons)
  (lm:vector (car cons) (cdr cons) 0))
