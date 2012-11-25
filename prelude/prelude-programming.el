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
(defun flymake-error-under-point ()
  "Display the flymake error under the point in the minibuffer."
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
        (let ((err (car (nth 1 elem))))
        (message "%s" (flymake-ler-text err)))))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Show the next error in the ring in the minibuffer."
  (flymake-error-under-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Show the previous error in the ring in the minibuffer."
  (flymake-error-under-point))

(defadvice flycheck-mode (before post-command-stuff activate compile)
  "When there's an error under the point, display it in the
minibuffer."
  (set (make-local-variable 'post-command-hook)
       (cons 'flymake-error-under-point post-command-hook)))

;; Enable flymake for all files
(add-hook 'find-file-hook 'flycheck-mode)

(provide 'prelude-programming)
