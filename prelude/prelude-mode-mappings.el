;;; prelude-mode-mappings.el --- Emacs Prelude: minor mode

;; Go
(autoload 'go-mode "go-mode")
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("jsTestDriver\\.conf$" . yaml-mode))

;; Jade and Stylus (sws = significant whitespace)
(autoload 'stylus-mode "stylus-mode")
(autoload 'jade-mode "jade-mode")
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))

;; Ruby
(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; JavaScript
; (autoload 'js2-mode "js2-mode" nil t)
; (autoload 'js3-mode "js3-mode" nil t)
; (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
; (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
; (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js3-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (define-key markdown-mode-map (kbd "<tab>") 'yas/expand)))

;; Org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(provide 'prelude-mode-mappings)
