(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;; add new pairs
(sp-add-pair "*" "*")
(sp-add-pair "$" "$")
(sp-add-pair "<" ">") ;; in html only!

;;; global
(sp-add-ban-insert-pair-in-string "'")

;; you can also use the `sp-with-tag' macro. It will automatically add
;; the tag to each function. Use this only with functions where the
;; first argument is the opening pair! Here, we want to disable ' pair
;; in a bunch of text modes
(sp-with-tag "'"
  (sp-add-local-ban-insert-pair 'markdown-mode)
  (sp-add-local-ban-insert-pair 'tex-mode)
  (sp-add-local-ban-insert-pair 'latex-mode)
  (sp-add-local-ban-insert-pair 'text-mode)
  (sp-add-local-ban-insert-pair 'log-edit-mode)
  (sp-add-local-ban-insert-pair 'org-mode))

;; now, we could've also done just this:
;; (sp-add-local-ban-insert-pair "'"
;;                               '(markdown-mode
;;                                 ...))
;; but I wanted to show you how to use the sp-with-tag macro :)

;;; emacs-lisp-mode(s)
(sp-with '(emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode)
  (sp-add-local-ban-insert-pair "'")
  (sp-add-local-ban-insert-pair-in-code "`"))

;;; markdown-mode
;; you can also use the `sp-with' macro. It will automatically add the
;; mode to the end of each call. How cool is that!
(sp-with '(markdown-mode rst-mode)
  (sp-add-local-pair "`" "`")
  ;; this also disables '*' in all other modes
  (sp-add-local-allow-insert-pair "*")
  (sp-add-tag-pair "2" "**" "**" nil))

;;; tex-mode latex-mode
(sp-with '(tex-mode latex-mode) ;; yes, this works with lists too!
  (sp-add-local-allow-insert-pair "$")
  (sp-add-tag-pair "i" "\"<" "\">" nil))

;;; python-mode
(sp-with 'python-mode
  (sp-add-local-ban-insert-pair "`"))

;;; html-mode
(sp-with '(html-mode sgml-mode)
  (sp-add-local-allow-insert-pair "<"))

(provide 'init-smartparens)
