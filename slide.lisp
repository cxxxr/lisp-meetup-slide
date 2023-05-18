(defpackage :lem-slide
  (:use :cl
        :alexandria
        :lem
        :lem-sdl2)
  (:export :define-page
           :define-slide))
(in-package :lem-slide)

(defvar *current-slide* nil)
(defvar *slides* '())

(defclass page ()
  ((name :initarg :name
         :accessor page-name)
   (index :initarg :index
          :accessor page-index)
   (buffer :initarg :buffer
           :initform nil
           :accessor page-buffer)))

(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "~A ~D" (page-name page) (page-index page))))

(defun page-buffer-name (page)
  (format nil "*slide ~A ~D*" (page-name page) (page-index page)))

(defun buffer-page (buffer)
  (buffer-value buffer 'page))

(defun (setf buffer-page) (page buffer)
  (setf (buffer-value buffer 'page) page))

(defun register-page (name)
  name)

(defun make-pages (page-names)
  (loop :for name :in page-names
        :for index :from 0
        :collect (make-instance 'page
                                :name name
                                :index index)))

(defun make-page-buffer (page)
  (when (and (page-buffer page)
             (not (deleted-buffer-p (page-buffer page))))
    (delete-buffer (page-buffer page)))
  (let ((buffer (make-buffer (page-buffer-name page))))
    (setf (page-buffer page) buffer)
    (setf (buffer-page buffer) page)
    (change-buffer-mode buffer 'slide-mode)
    (setf (buffer-read-only-p buffer) t)))

(defun draw-page (page)
  (let ((buffer (page-buffer page)))
    (funcall (page-name page)
             buffer)))

(defclass slide ()
  ((pages :initarg :pages
          :initform '()
          :accessor slide-pages)))

(defun find-slide (slide-name)
  (when (find slide-name *slides*)
    (make-instance slide-name)))

(defun ensure-slide (slide)
  (etypecase slide
    (slide slide)
    (symbol (or (find-slide slide)
                (editor-error "Slide not found: ~A" slide)))))

(defun start-slide (slide)
  (let ((slide (ensure-slide slide)))
    (mapc #'make-page-buffer (slide-pages slide))
    (mapc #'draw-page (slide-pages slide))
    (switch-to-page (first (slide-pages slide)))))

(defun get-next-page (slide page)
  (let ((pos (position page (slide-pages slide))))
    (cond ((null pos) nil)
          ((< (1+ pos) (length (slide-pages slide)))
           (elt (slide-pages slide) (1+ pos)))
          (t nil))))

(defun get-previous-page (slide page)
  (let ((pos (position page (slide-pages slide))))
    (cond ((null pos) nil)
          ((zerop pos) nil)
          (t (elt (slide-pages slide) (1- pos))))))

(defun switch-to-page (page)
  (switch-to-buffer (page-buffer page)))

(defmacro define-page (name (buffer) &body body)
  `(progn
     (defun ,name (,buffer) ,@body)
     (register-page ',name)))

(defmacro define-slide (name (&rest page-names))
  `(progn
     (pushnew ',name *slides*)
     (defclass ,name (slide)
       ()
       (:default-initargs
        :pages (make-pages ',page-names)))
     (setf *current-slide* (make-instance ',name))))

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
  (when-let ((page (get-next-page *current-slide* (buffer-page (current-buffer)))))
    (switch-to-page page)))

(define-command slide-previous () ()
  (when-let ((page (get-previous-page *current-slide* (buffer-page (current-buffer)))))
    (switch-to-page page)))

(define-command slide-start () ()
  (start-slide *current-slide*))
