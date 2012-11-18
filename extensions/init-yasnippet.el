;; Use only own snippets, do not use bundled ones
(require 'yasnippet)
(add-to-list 'yas/snippet-dirs prelude-snippets-dir)
(yas/global-mode 1)

(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; Jump to end of snippet definition
(define-key yas/keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
        (position (yas/field-end (yas/snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas/snippets-at-point)))
        (position (yas/field-start (yas/snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line)
      (goto-char position))))

(define-key yas/keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas/keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; No dropdowns please, yas
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/completing-prompt))

;; Wrap around region
(setq yas/wrap-around-region t)

;; Replace yasnippets's TAB
(add-hook 'yas/minor-mode-hook
    (lambda () (define-key yas/minor-mode-map
        (kbd "TAB") 'smart-tab))) ; was yas/expand

(provide 'init-yasnippet)
