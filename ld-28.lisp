;;;; ld-28.lisp

(in-package #:ld-28)

(defvar *screen*)
(defparameter *background-color* sdl:*black*)  ; we update this when a collision is detected

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
  ((shape :accessor shape               ; shape is in object coordinates, 
          :initarg :shape)              ; so the origin is the shape's pivot point
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
  (setf *background-color*
        (if (collision? (player screen) (enemy screen))
            (sdl:color :r 150 :g 50 :b 50)
            sdl:*black*)))

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
  (sdl:clear-display *background-color*)
  (sdl:with-color (_ (sdl:color :r 90 :g 90 :b 90))
    ;; border around the game so I can see the edges w/ my messed up windowing...
    (sdl:draw-rectangle-* 0 0 (window-width) (window-height))
    ;; short in-game instructions
    (sdl:draw-string-solid-* "Player: WASD & Q/W" 10 10)
    (sdl:draw-string-solid-* "Enemy: Mouse & Mouse Wheel" 10 20)
    (sdl:draw-string-solid-* "<ESC> to quit" (- (window-width) 120) (- (window-height) 18)))
  (with-accessors ((p player) (e enemy)) screen
    (draw-shape (mapcar (lambda (v) (lm:* (transform p) v))
                        (shape p)))
    (draw-shape (mapcar (lambda (v) (lm:* (lm:create-translation-matrix (mouse-in-world-coords))
                                          (transform e)
                                          v))
                        (shape e)))))

(defun draw-shape (vertices)
  (sdl:draw-polygon (mapcar (lambda (v)
                              (vector->point
                               ;; Add half the window dimensions to center origin
                               (lm:+ v (lm:vector (/ (window-width) 2) (/ (window-height) 2) 0))))
                            vertices)))

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
        (w (window-width))
        (h (window-height)))
    (list (- x (/ w 2))
          (- y (/ h 2)))))

(defun window-width ()
  (aref (sdl:video-dimensions) 0))

(defun window-height ()
  (aref (sdl:video-dimensions) 1))

;;; gives us the unit 2-vector normal
(defun normal (vector)
  (lm:normalise (lm:vector (- (lm:y vector)) (lm:x vector))))

;;; Makes a line 2-vector out two 3-vector points
(defun make-line (p1 p2)
  (lm:to-vector (lm:- p1 p2) :dimension 2))

;;; gives length of a shape when projected onto (unit) vector
(defun projected-length (shape vector)
  (let ((lengths (mapcar (lambda (point)
                           (lm:dot-product point (lm:to-vector vector :dimension 3)))
                         shape)))
    (- (apply 'max lengths) (apply 'min lengths))))

(defun collision? (entity1 entity2)
  (with-accessors ((shape1 shape) (t1 transform)) entity1
    (with-accessors ((shape2 shape) (t2 transform)) entity2
      (let* ((s1 (mapcar (lambda (v) (lm:* t1 v)) shape1))
             (s2 (mapcar (lambda (v) (lm:* (lm:create-translation-matrix (mouse-in-world-coords))
                                           t2
                                           v))
                         shape2))
             (lines1 (mapcar 'make-line
                             s1
                             (append (cdr s1) (list (car s1)))))
             (lines2 (mapcar 'make-line
                             s2
                             (append (cdr s2) (list (car s2)))))
             (normals1 (mapcar 'normal lines1))
             (normals2 (mapcar 'normal lines2))
             (center1 (shape-center s1))
             (center2 (shape-center s2)))
        (loop
           :for n in (append normals1 normals2)
           :if (> (lm:dot-product (lm:to-vector (lm:- center1 center2) :dimension 2) (lm:to-vector n))
                  (/ (+ (projected-length s1 n)
                        (projected-length s2 n))
                     2))
           :return nil
           :finally (return t))))))
