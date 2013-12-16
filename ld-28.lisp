;;;; ld-28.lisp

(in-package #:ld-28)

;;; Only global thus far!
(defvar *screen*)

;;; CLASSES
;;; =======

(defclass menu-screen ()
  ((options :accessor menu-items
            :initarg :options
            :initform '("Play"
                        "Do nothing"
                        "Quit"))
   (selection :accessor menu-selection
              :initarg :selection
              :initform 0)))

(defclass game-screen ()
  ((level :accessor game-level
          :initarg :level
          :initform 0)))

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
  (sdl:with-color (col (sdl:color :r 100 :g 100 :b 100))
    (loop :for i :from 0
       :for s :in '("Press Right to win the level!"
                    "Left to go back to previously beat levels."
                    "Escape to go back to menu.")
       :do (sdl:draw-string-solid-* s 10 (* 10 (1+ i)))))
  
  (sdl:with-font (font (make-instance 'SDL:ttf-font-definition
                                      :size 72
                                      :filename sdl:*default-ttf-font*))
    (sdl:draw-string-solid-* (format nil "Level ~A" (game-level screen))
                             10
                             (- (/ (aref (sdl:video-dimensions) 1) 2) 36))))

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
    (:sdl-key-escape (setf *screen* (make-instance 'menu-screen)))
    (:sdl-key-left (setf (game-level screen) (max 0 (1- (game-level screen)))))
    (:sdl-key-right (incf (game-level screen)))))

;;; MAIN LOOP
;;; =========

(defun run (&key (width 460) (height 284)) ; golden ratio yo
  (setf *screen* (make-instance 'menu-screen))
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
