;;;; ld-28.lisp

(in-package #:ld-28)

(defun run ()
  (sdl:with-init ()
    (sdl:window 320 198 :title-caption "LD 28: You Only Get One")
    (setf (sdl:frame-rate) 1)
    (sdl:initialise-default-font)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
        (when (sdl:key= key :sdl-key-escape)
          (sdl:push-quit-event)))
      (:idle ()
        (render)
        (sdl:update-display)))))

(defun render ()
  (sdl:clear-display (sdl:color :r 120 :g 50 :b 50))
  (sdl:draw-box-* 3 3 314 192 :color sdl:*black*)
  (let ((title "You Only Get One")
        (x-margin 18))
    (sdl:draw-string-solid-* title x-margin (- 198/2 9))
    (sdl:draw-hline x-margin
                    (+ x-margin (* 8 (length title)))
                    198/2
                    :color sdl:*red*)))
