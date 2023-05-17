(defpackage :lem-lispmeetup-slide
  (:use :cl
        :alexandria
        :lem
        :lem-sdl2))
(in-package :lem-lispmeetup-slide)

(defun path (filename)
  (asdf:system-relative-pathname :lem-lispmeetup-slide filename))

(defun buffer-page-index (buffer)
  (buffer-value buffer 'page-index))

(defun (setf buffer-page-index) (page-index buffer)
  (setf (buffer-value buffer 'page-index) page-index))

(defun page-buffer-name (page-index)
  (format nil "*slide ~D*" page-index))

(defun find-page-buffer (page-index)
  (get-buffer (page-buffer-name page-index)))

(defun call-with-page (page-index function)
  (let ((buffer-name (page-buffer-name page-index)))
    (when-let (buffer (get-buffer buffer-name))
      (delete-buffer buffer))
    (let ((buffer (make-buffer buffer-name)))
      (setf (buffer-page-index buffer) page-index)
      (change-buffer-mode buffer 'slide-mode)
      (funcall function buffer))))

(defvar *page-names* '())

(defmacro def-page ((name page-index) (buffer) &body body)
  `(progn
     (pushnew ',name *page-names*)
     (defun ,name ()
       (call-with-page ,page-index (lambda (,buffer) ,@body)))))

(defvar *h1-font*
  (sdl2-ttf:open-font (path "NotoSansCJK-Regular.ttc") 300))

(defvar *h2-font*
  (sdl2-ttf:open-font (path "NotoSansCJK-Regular.ttc") 150))

(defvar *normal-font*
  (sdl2-ttf:open-font (path "NotoSansCJK-Regular.ttc") 100))

(defvar *emoji-font*
  (sdl2-ttf:open-font (path "NotoColorEmoji.ttf") 20))

(defvar *foreground-color* (make-color 255 255 255))

(defvar *icon* (load-image (path "icon.png")))

(defvar *discord-qrcode* (load-image (path "discord.png")))
(defvar *opencollective-qrcode* (load-image (path "opencollective.png")))

(defun draw-icon (buffer)
  (draw-image buffer
              *icon*
              :x 1800
              :y 50
              :width 500
              :height 500))

(defun draw-list (buffer strings)
  (loop :for string :in strings
        :for y := 260 :then (+ y 200)
        :do (draw-string buffer
                         string
                         0
                         y
                         :font *normal-font*
                         :color *foreground-color*)))

(def-page (page-1 1) (buffer)
  (draw-icon buffer)
  (draw-string buffer
               "Lem 2.0"
               40
               0
               :font *h1-font*
               :color *foreground-color*)
  (draw-string buffer
               "2023-05-27"
               50
               1100
               :font *normal-font*
               :color *foreground-color*))

(def-page (page-2 2) (buffer)
  (draw-icon buffer)
  (draw-string buffer
               "Lemとは"
               40
               0
               :font *h2-font*
               :color *foreground-color*)
  (draw-list buffer
             '("・Common Lisp IDE"
               "・Common Lisp製のEmacs Like Editor"
               "・Language Server Protocol(LSP)のサポート"
               "・その他GoやRust, Cなど30の言語に対応")))

(def-page (page-3 3) (buffer)
  (draw-icon buffer)
  (draw-string buffer
               "2.0の新機能"
               40
               0
               :font *h2-font*
               :color *foreground-color*)
  (draw-list buffer
             '("    SDL2 frontend"
               "    ・Graphics"
               "    ・マウスサポート"
               "    ・windowsサポート")))

(def-page (page-4 4) (buffer)
  (draw-icon buffer)
  (draw-string buffer
               "2.0の新機能"
               40
               0
               :font *h2-font*
               :color *foreground-color*)
  (draw-list buffer
             '("    Look & Feel"
               "    ・Color Themeを180以上追加"
               "    ・grep, シンボル定義への移動等のUIの改善"
               "    ・Multi column list component"
               "    ・Powerline")))

(def-page (page-5 5) (buffer)
  (draw-icon buffer)
  (draw-string buffer
               "次にすること"
               40
               0
               :font *h2-font*
               :color *foreground-color*)
  (draw-list buffer
             '("    ・Lisp ModeとLSPの統合"
               "    ・Lisp ModeとGraphics機能の統合"
               "    ・Trial(ゲームエンジン)との統合")))

(def-page (page-6 6) (buffer)
  (draw-string buffer
               "おまけ"
               800
               500
               :font *h2-font*
               :color *foreground-color*))

(def-page (page-7 7) (buffer)
  (draw-string buffer
               "🙏🍺"
               1650
               50
               :color (make-color 0 0 0)
               :font *emoji-font*)
  (draw-string buffer
               "Contribute & Feedback"
               40
               0
               :font *h2-font*
               :color *foreground-color*)
  (draw-string buffer
               "Discord"
               350
               350
               :font *normal-font*
               :color *foreground-color*)
  (draw-image buffer
              *discord-qrcode*
              :x 300
              :y 500)
  (draw-string buffer
               "open collective"
               1400
               350
               :font *normal-font*
               :color *foreground-color*)
  (draw-image buffer
              *opencollective-qrcode*
              :x 1500
              :y 500))

(define-major-mode slide-mode ()
    (:name "Slide"
     :keymap *slide-mode-keymap*))

(define-key *slide-mode-keymap* "Right" 'slide-next)
(define-key *slide-mode-keymap* "Left" 'slide-previous)
(define-key *slide-mode-keymap* "n" 'slide-next)
(define-key *slide-mode-keymap* "p" 'slide-previous)
(define-key *slide-mode-keymap* "Space" 'slide-next)
(define-key *slide-mode-keymap* "Backspace" 'slide-previous)

(define-command slide-next () ()
  (when-let* ((page-index (buffer-page-index (current-buffer)))
              (buffer (find-page-buffer (1+ page-index))))
    (switch-to-buffer buffer)))

(define-command slide-previous () ()
  (when-let* ((page-index (buffer-page-index (current-buffer)))
              (buffer (find-page-buffer (1- page-index))))
    (switch-to-buffer buffer)))

(define-command slide () ()
  (mapc #'funcall *page-names*))
