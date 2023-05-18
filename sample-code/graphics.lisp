(defpackage :graphics-example
  (:use :cl
        :lem
        :lem-sdl2))
(in-package :graphics-example)

(defun random-color ()
  (lem:make-color (random 256) (random 256) (random 256)))

(define-command graphics-example () ()
  (loop :repeat 100
        :for x := (random 500)
        :for y := (random 500)
        :do (draw-rectangle (current-window)
                            x
                            y
                            100
                            100
                            :filled t
                            :color (random-color))
            (sit-for 0.01)))
