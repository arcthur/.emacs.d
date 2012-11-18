;; paredit (plugin)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
  (mapc (lambda (mode)
    (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
     (add-hook hook (lambda () (paredit-mode 1)))))
    '(emacs-lisp lisp inferior-lisp slime slime-repl))

(defun conditionally-enable-paredit-mode ()
  "Enable paredit-mode in the minibuffer, during eval-expression."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(provide 'init-paredit)
