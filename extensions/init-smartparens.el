;;;;;;;;;
;; global
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

;;;;;;;;;;;;;;;;;;
;; pair management

;;; markdown-mode
(sp-add-pair "*" "*")

;; you can use the `sp-with' macro. It will automatically add the
;; _mode_ to the end of each call. How cool is that!
(sp-with '(markdown-mode gfm-mode rst-mode)
  ;; this also disables '*' in all other modes
  (sp-add-local-allow-insert-pair "*")
  (sp-add-tag-pair "2" "**" "**" nil)
  (sp-add-tag-pair "s" "```scheme" "```" nil)
  (sp-add-tag-pair "<"  "<_>" "</_>" 'sp-match-sgml-tags))

;; Besides the `sp-with' macro, when applying various permissions on
;; the same tag, you can also use the `sp-with-tag' macro. It will
;; automatically add the _tag_ to each function. Use this only with
;; functions where the first argument is the opening pair! Here, we
;; want to apply several permissions on the '' pair:
;;
;; (sp-with-tag "'"
;;   (sp-add-local-ban-insert-pair 'text-mode)
;;   (sp-add-local-ban-insert-pair-in-string 'c-mode))

;;; tex-mode latex-mode
(sp-with '(tex-mode plain-tex-mode latex-mode)
  (sp-add-tag-pair "i" "1fe989d0dcc740eac417410c05794857765a1e2fquot;<" "1fe989d0dcc740eac417410c05794857765a1e2fquot;>" nil))

;;; python-mode
(sp-with 'python-mode
  (sp-add-local-ban-insert-pair "`"))

;;; html-mode
(sp-add-pair "<" ">") ;; in html only!
(sp-with '(html-mode sgml-mode)
  (sp-add-local-allow-insert-pair "<"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom defuns and macros
(defun my-wrap-with-paren (&optional arg)
  "Select ARG next things and wrap them with a () pair."
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  ;; simulate user pressing '(' key
  (execute-kbd-macro (kbd "(")))
(define-key sp-keymap (kbd "C-(") 'my-wrap-with-paren)

(provide 'init-smartparens)
