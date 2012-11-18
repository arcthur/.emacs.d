;; Volatile Highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Hightlight Symbol
(custom-set-variables
   '(highlight-symbol-idle-delay 0.5)
   '(highlight-symbol-on-navigation-p t)
   '(hl-paren-colors (quote ("firebrick1" "IndianRed1" "IndianRed4" "grey")))
   '(pulse-delay 0.03)
   '(pulse-flag nil)
   '(pulse-iterations 5))

(defun highlight-symbol-mode-on ()
"Turn on function `highlight-symbol-mode'."
    (highlight-symbol-mode 1))
(defun highlight-symbol-mode-off ()
"Turn off function `highlight-symbol-mode'."
    (highlight-symbol-mode -1))

(dolist
  (my-hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook text-mode-hook ruby-mode-hook html-mode-hook))
  (add-hook my-hook 'highlight-symbol-mode-on))

;; Enables highlight-parentheses-mode on all buffers
(define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Rainbow Delimiter
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(setq-default frame-background-mode 'dark)

(provide 'init-highlight)
