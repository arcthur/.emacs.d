(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with '(
           emacs-lisp-mode
           inferior-emacs-lisp-mode
           lisp-interaction-mode
           scheme-mode
           common-lisp-mode
           )
  ;; disable ' everywhere, it's the quote character!
  (sp-add-local-ban-insert-pair "'")
  ;; also disable the pseudo-quote inside code.  We keep it in
  ;; commends and strings for hyperlinks
  (sp-add-local-ban-insert-pair-in-code "`"))

;; markdown based modes
(sp-with '(
           markdown-mode
           gfm-mode
           rst-mode
           )
  ;; overload the `' pair with ``, which is used for inline
  ;; code in markdown
  (sp-add-local-pair "`" "`"))

;; LaTeX modes
(sp-add-pair "$" "$")
(sp-with '(
           tex-mode
           plain-tex-mode
           latex-mode
           )
  ;; allow the dollar pair only in LaTeX related modes.  It
  ;; often marks a variable elsewhere
  (sp-add-local-allow-insert-pair "$"))

(provide 'init-smartparens)
