(require 'init-latex-mode)
(require 'init-html-mode)
(require 'init-css-mode)
(require 'init-stylus-mode)
(require 'init-coffee-mode)
(require 'init-org-mode)
(require 'init-magit)
(require 'init-markdown-mode)
(require 'init-ruby-mode)
(require 'init-js-mode)

;; Regular expressions
;; ----------------------------------------------------------
(require 're-builder)
(setq reb-re-syntax 'string)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Flymake
;; ----------------------------------------------------------
;; Enable flymake for all files
(require 'init-flycheck)

(provide 'prelude-programming)
