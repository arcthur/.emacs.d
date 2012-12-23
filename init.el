;; -*- coding: utf-8 -*-
(setq emacs-load-start-time (current-time))

(setq exec-path (append exec-path '("/usr/local/bin" "/usr/bin" "/usr/texbin" "/usr/local/share/npm/bin")))
(setenv "PATH" (concat "/usr/texbin:" "/usr/local/bin:" (getenv "PATH")))

;;; Initialization
(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-modules-dir (expand-file-name  "prelude" prelude-dir)
  "This folder houses all of the built-in Prelude module. You should
avoid modifying the configuration there.")
(defvar prelude-defuns-dir (expand-file-name "defuns" prelude-dir)
  "This folder house some util defunctions.")
(defvar prelude-modes-dir (expand-file-name "modes" prelude-dir)
  "This folder houses programming modes.")
(defvar prelude-extensions-dir (expand-file-name "extensions" prelude-dir)
  "This folder houses extensions configuration.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This folder houses Emacs Lisp packages.")
(defvar prelude-themes-dir (expand-file-name "themes" prelude-dir)
  "This folder sotres custome theme.")
(defvar prelude-snippets-dir (expand-file-name "snippets" prelude-dir)
  "This folder houses addition yasnippet bundles.")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

;; add Prelude's directories to Emacs's `load-path'
(setq el-get-dir prelude-vendor-dir)
(add-to-list 'load-path (concat prelude-vendor-dir "/el-get"))
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-defuns-dir)
(add-to-list 'load-path prelude-extensions-dir)
(add-to-list 'load-path prelude-modes-dir)
(add-to-list 'custom-theme-load-path prelude-themes-dir)

(require 'prelude-packages)

;; Functions (load all files in prelude-defuns-dir)
(dolist (file (directory-files prelude-defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'prelude-ui)
(require 'prelude-modeline)
(require 'prelude-misc)
(require 'prelude-editor)
(require 'prelude-keybindings)
(require 'prelude-programming)

(when (eq system-type 'darwin)
  (require 'prelude-osx))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Fullscreen
(ns-toggle-fullscreen)

(when (require 'time-date nil t)
   (message "Emacs startup time: %d seconds."
    (time-to-seconds (time-since emacs-load-start-time))))
