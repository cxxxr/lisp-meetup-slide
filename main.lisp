(defpackage :lem-lispmeetup-slide
  (:use :cl
        :alexandria
        :lem
        :lem-sdl2))
(in-package :lem-lispmeetup-slide)

(defun path (filename)
  (asdf:system-relative-pathname :lem-lispmeetup-slide (merge-pathnames filename "resources/")))

(defvar *h1-font*
  (sdl2-ttf:open-font (path "NotoSansCJK-Regular.ttc") 300))

(defvar *h2-font*
  (sdl2-ttf:open-font (path "NotoSansCJK-Regular.ttc") 150))

(defvar *normal-font*
  (sdl2-ttf:open-font (path "NotoSansCJK-Regular.ttc") 100))

(defvar *emoji-font*
  (sdl2-ttf:open-font (path "NotoColorEmoji.ttf") 20))

(defvar *foreground-color* (make-color 255 255 255))

(defvar *icon-image* (load-image (path "icon.png")))

(defvar *cloc-image* (load-image (path "cloc.png")))
(defvar *github-contributors-image* (load-image (path "contributors.png")))

(defvar *architecture-image* (load-image (path "architecture.png")))

(defvar *discord-qrcode-image* (load-image (path "discord.png")))
(defvar *opencollective-qrcode-image* (load-image (path "opencollective.png")))

(defun draw-icon (buffer)
  (draw-image buffer
              *icon-image*
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

(defun draw-header (buffer string &key (font *h2-font*))
  (draw-string buffer
               string
               40
               0
               :font font
               :color *foreground-color*))

(lem-slide:define-page start-page (buffer)
  (draw-icon buffer)
  (draw-header buffer
               "Lem 2.0"
               :font *h1-font*)
  (draw-string buffer
               "Lisp meetup #109"
               50
               1100
               :font *normal-font*
               :color *foreground-color*)
  (draw-string buffer
               "2023-05-27"
               50
               1250
               :font *normal-font*
               :color *foreground-color*)
  (draw-string buffer
               "cxxxr"
               1800
               1200
               :font *normal-font*
               :color *foreground-color*))

(lem-slide:define-page agenda-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "はじめに")
  (draw-list buffer
             '("・Lemについての紹介"
               "・ver 2.0で追加した機能について"
               "・動かしてみる"
               "・今後の展望")))

(lem-slide:define-page about-lem-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "Lemとは")
  (draw-list buffer
             '("・Common Lisp IDE"
               "・Common Lisp製のEmacs Like Editor"
               "・ExtensionもComon Lispで書く"
               "・Language Server Protocol(LSP)のサポート")))

(lem-slide:define-page about-lem-page-2 (buffer)
  (draw-icon buffer)
  (draw-header buffer "Lemとは")
  (draw-list buffer
             '("・自分が日常的に使えるもの"
               "・Goなどにも対応"
               "・対応言語数はおよそ30個")))

(lem-slide:define-page lem-scale-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "コードの規模")
  (draw-image buffer
              *cloc-image*
              :x 10
              :y 300)
  (draw-image buffer
              *github-contributors-image*
              :x 1050
              :y 700))

(lem-slide:define-page lem-architecture-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "構成")
  (draw-image buffer
              *architecture-image*
              :x 10
              :y 300)
  (draw-string buffer
               "Frontend"
               1400
               600
               :font *normal-font*
               :color *foreground-color*)
  (draw-string buffer
               "- Terminal"
               1500
               750
               :font *normal-font*
               :color *foreground-color*)
  (draw-string buffer
               "- jsonrpc"
               1500
               900
               :font *normal-font*
               :color *foreground-color*)
  (draw-string buffer
               "- SDL2 (new!)"
               1500
               1050
               :font *normal-font*
               :color *foreground-color*))

(lem-slide:define-page add-2.0-sdl2-frontend-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "2.0の新機能")
  (draw-list buffer
             '("    SDL2 frontend"
               "    ・Graphics"
               "    ・マウスサポート"
               "    ・windowsサポート")))

(lem-slide:define-page add-2.0-look-and-feel-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "2.0の新機能")
  (draw-list buffer
             '("    Look & Feel"
               "    ・Color Themeを180以上追加"
               "    ・grep, シンボル定義への移動等のUIの改善"
               "    ・Multi column list component"
               "    ・Powerline")))

(lem-slide:define-page bugs-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "辛い問題")
  (draw-list buffer
             '("    ・修飾キーの取り扱い"
               "    ・キーボード配列の扱い(JIS/US/etc...)"
               "    ・キーイベントがOSによって違う"
               "    ・フォント一覧の取得もOSによって違う")))

(lem-slide:define-page bugs-page-2 (buffer)
  (draw-icon buffer)
  (draw-header buffer "辛い問題(2)")
  (draw-list buffer
             '("    ・GPUのドライバによっては動かない"
               "    ・サスペンドから復帰すると死ぬ")))

(lem-slide:define-page what-is-next-page (buffer)
  (draw-icon buffer)
  (draw-header buffer "次にすること")
  (draw-list buffer
             '("    ・Lisp ModeとLSPの統合"
               "    ・Lisp ModeとGraphics機能の統合"
               "    ・Trial(ゲームエンジン)との統合"
               "    ・ドキュメントの整備")))

(lem-slide:define-page contribute-and-feedback-page (buffer)
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
              *discord-qrcode-image*
              :x 300
              :y 500)
  (draw-string buffer
               "open collective"
               1400
               350
               :font *normal-font*
               :color *foreground-color*)
  (draw-image buffer
              *opencollective-qrcode-image*
              :x 1500
              :y 500))

(lem-slide:define-slide lem-v2.0
  (start-page
   agenda-page
   about-lem-page
   about-lem-page-2
   lem-scale-page
   lem-architecture-page
   add-2.0-sdl2-frontend-page
   add-2.0-look-and-feel-page
   bugs-page
   bugs-page-2
   what-is-next-page
   contribute-and-feedback-page))
