;; Basic
;; ----------------------------------------------------------
;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

(setq color-theme-is-global t
      truncate-partial-width-windows nil)

;; Apply syntax highlighting to all buffers
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Less flickery display
(setq redisplay-dont-pause t)

;; Highlight the current line
(global-hl-line-mode t)

;; Disable blink cursor
(blink-cursor-mode -1)

;; No border
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; ModeLine
;; ----------------------------------------------------------
;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)

;; Theme
;; ----------------------------------------------------------
;; Load theme extensions
(load-theme 'graphene t)
(defadvice load-theme
    (after load-graphene-theme (theme &optional no-confirm no-enable) activate)
      "Load the graphene theme extensions after loading a theme."
      (when (not (equal theme 'graphene))
      (load-theme 'graphene t)))

;; Use tomorrow as the default theme
;; (color-theme-sanityinc-tomorrow-eighties)
(load-theme 'solarized-dark)

;; Scrolling
;; ----------------------------------------------------------
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Fringe
;; ----------------------------------------------------------
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Frame Title
;; ----------------------------------------------------------
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; Transparent
;; ----------------------------------------------------------
(set-frame-parameter (selected-frame) 'alpha '(95 90))
(add-to-list 'default-frame-alist '(alpha 95 90))

;; Font
;; ----------------------------------------------------------
(add-to-list 'default-frame-alist '(line-spacing . 2))
(set-face-font 'default "Menlo-12")
(set-face-font 'variable-pitch "Lucida Sans-12")
(set-face-font 'fixed-pitch "Menlo-12")

(provide 'prelude-ui)
