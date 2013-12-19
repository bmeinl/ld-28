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

;;; If we pass :auto-vectorize t to make-instance 'entity, we can pass
;;; :shape as list of (x . y) conses and it'll convert them to
;;; lm:vectors for us
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
  (with-accessors ((p player)) screen
    (draw-shape (mapcar (lambda (v) (lm:* (transform p) v))
                        (shape p)))))

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
  (with-accessors ((trans transform)) (player *screen*)
    (case key
      (:sdl-key-escape (sdl:push-quit-event))
      (:sdl-key-up (setf trans(lm:* trans (lm:create-translation-matrix (list 0 (- +speed+))))))
      (:sdl-key-down (setf trans(lm:* trans (lm:create-translation-matrix (list 0 +speed+)))))
      (:sdl-key-left (setf trans(lm:* trans (lm:create-translation-matrix (list (- +speed+) 0)))))
      (:sdl-key-right (setf trans(lm:* trans (lm:create-translation-matrix (list +speed+ 0))))))))

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
  (lm:vector (car cons) (cdr cons) 1))

(defun vector->point (vector)
  (sdl:point :x (lm:x vector) :y (lm:y vector)))
