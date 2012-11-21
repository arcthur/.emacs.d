(require 'init-latex-mode)
(require 'init-html-mode)
(require 'init-css-mode)
(require 'init-stylus-mode)
(require 'init-coffee-mode)
(require 'init-org-mode)
(require 'init-magit)
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
(autoload 'flymake-find-file-hook "flymake" "" t)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq flymake-gui-warnings-enabled nil)
(setq flymake-run-in-place t)
;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)
(eval-after-load 'flymake '(require 'flymake-cursor))

;; Stop flymake from breaking when ruby-mode is invoked by mmm-mode,
;; at which point buffer-file-name is nil
(eval-after-load 'flymake
  '(progn
     (global-set-key (kbd "C-`") 'flymake-goto-next-error)

     (defun flymake-can-syntax-check-file (file-name)
       "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
       (if (and file-name (flymake-get-init-function file-name)) t nil))))

;; flycheck
; (add-hook 'tex-mode-hook 'flycheck-mode)

(provide 'prelude-programming)
