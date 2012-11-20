(autoload 'gtags-mode "gtags" "" t)

(defun setup-global-mode-for (mode)
 (add-hook mode (lambda ()
        (gtags-mode 1)
     (diminish 'gtags-mode "gt")
        (setq gtags-symbol-regexp "[A-Za-z_:][A-Za-z0-9_#.:?]*"))))

(mapc 'setup-global-mode-for
      '(ruby-mode-hook
        js2-mode-hook))

(provide 'init-gtags)
