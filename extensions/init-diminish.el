(require 'diminish)
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode "ut"))

(eval-after-load "eproject"
  '(diminish 'eproject-mode "eprj"))

(eval-after-load "Projectile"
  '(diminish 'projectile-mode "prj"))

(eval-after-load "volatile-highlights"
  '(diminish 'volatile-highlights-mode "vh"))

(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode "ac"))

(eval-after-load "highlight-parentheses-mode"
  '(diminish 'highlight-parentheses-mode "hp"))

(eval-after-load "yas/minor-mode"
  '(diminish 'yas/minor-mode "yas"))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "elisp")))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq mode-name "js2")))

(provide 'init-diminish)
