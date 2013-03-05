;; keybinding to toggle full screen mode
;; ----------------------------------------------------------
(global-set-key [(f1)] 'ns-toggle-fullscreen)

;; Find stuff
;; ----------------------------------------------------------
(global-set-key [(f2)] 'ack)
;; Find files by name and display results in dired
(global-set-key [(meta f2)] 'find-name-dired)
;; Display and edit occurances of regexp in buffer
(global-set-key [(shift f2)] 'occur)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Highlight symbol
;; ----------------------------------------------------------
(global-set-key [(f3)]   'highlight-symbol-at-point)
(global-set-key [(meta f3)]   'highlight-symbol-prev)
(global-set-key [M-down] 'highlight-symbol-next)

;; Repeat complex command
;; ----------------------------------------------------------
(global-set-key [(f4)] 'repeat-complex-command)

;; Speedbar
;; ----------------------------------------------------------
(global-set-key [(f5)] (lambda()
          (interactive)
          (sr-speedbar-toggle)))

(global-set-key [(meta f5)] 'sr-speedbar-select-window)

;; Regex search & replace
;; ----------------------------------------------------------
(global-set-key [(f6)] 'replace-regexp)
(global-set-key [(f7)] 'isearch-forward-regexp)

;; Indenting and alignment
;; ----------------------------------------------------------
(global-set-key [(f8)]         'indent-region)
(global-set-key [(control f8)] 'align)
(global-set-key [(shift f8)]   'align-current)
(global-set-key [(meta f8)]    'align-regexp)

;; Calendar
;; ----------------------------------------------------------
(global-set-key [(f9)] 'calendar)

;; Bookmarks
;; ----------------------------------------------------------
(global-set-key [(f11)] 'list-bookmarks)
;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Help
;; ----------------------------------------------------------
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key [(f12)] 'help-command)

;; Font size
;; ----------------------------------------------------------
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Replace dired's M-o
;; ----------------------------------------------------------
;; map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically)   ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows)      ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window)             ; was digit-argument
(global-set-key (kbd "M-o") 'other-window)              ; was facemenu-keymap

(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'rotate-windows)
(global-set-key (kbd "C-x C--") 'toggle-window-split)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window

; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)

;; Use the preferrable delete-forward-char for interactive use
(global-set-key (kbd "C-d") 'delete-forward-char)

;; Expand region (increases selected region by semantic units)
;; ----------------------------------------------------------
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c w") (make-repeatable-command 'er/expand-region))

;; Multiple cursors
;; ----------------------------------------------------------
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)

;; Mark additional regions matching current region
;; ----------------------------------------------------------
(global-set-key (kbd "M->") 'mark-all-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M->") 'mark-more-like-this-extended)
(global-set-key (kbd "M-<") 'mark-all-in-region)

;; Set anchor to start rectangular-region-mode
;; ----------------------------------------------------------
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with inline-string-rectangle
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Ace jump mode
;; ----------------------------------------------------------
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Perform general cleanup.
;; ----------------------------------------------------------
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; Back to indentation - like ^ in Vim
;; ----------------------------------------------------------
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Transpose stuff
;; ----------------------------------------------------------
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Change next underscore with a camel case
;; ----------------------------------------------------------
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Killing text
;; ----------------------------------------------------------
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Copy-line if no active region
;; ----------------------------------------------------------
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") '(lambda () (interactive) (save-region-or-current-line 1)))

;; Shell
;; ----------------------------------------------------------
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x C-z") 'suspend-frame)

;; Zap to char
;; ----------------------------------------------------------
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; iy-go-to-char - like f in Vim
;; ----------------------------------------------------------
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; change-inner - like ci && co in Vim
;; ----------------------------------------------------------
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

;; Create new frame
;; ----------------------------------------------------------
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
;; ----------------------------------------------------------
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; ido/smex
;; ----------------------------------------------------------
(global-set-key (kbd "M-x") 'beautify-smex)
; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-X") 'helm-M-x)

;; File finding
;; ----------------------------------------------------------
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c y")   'bury-buffer)
(global-set-key (kbd "C-c r")   'revert-buffer)
(global-set-key (kbd "C-x t")   'visit-term-buffer)
(global-set-key (kbd "M-`")     'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-j") 'quick-switch-buffer)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>")
                (lambda () (interactive) (revert-buffer t t)))

;; Edit file with sudo
;; ----------------------------------------------------------
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path to kill ring
;; ----------------------------------------------------------
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; Window move
;; ----------------------------------------------------------
(global-set-key (kbd "M-p") 'window-move-up)
(global-set-key (kbd "M-n") 'window-move-down)

;; Indentation help
;; ----------------------------------------------------------
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Fetch the contents at a URL, display it raw.
;; ----------------------------------------------------------
(global-set-key (kbd "C-c C-u") 'view-url)

;; Should be able to eval-and-replace anywhere.
;; ----------------------------------------------------------
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Navigation bindings
;; ----------------------------------------------------------
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
; (global-set-key (kbd "M-p") 'backward-paragraph)
; (global-set-key (kbd "M-n") 'forward-paragraph)

;; Smart
;; ----------------------------------------------------------
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Webjump
;; ----------------------------------------------------------
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
;; ----------------------------------------------------------
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; isearch
;; ----------------------------------------------------------
;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)
(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; Like isearch-*-use-region, but doesn't fuck with the active region
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Move more quickly
;; ----------------------------------------------------------
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; Query replace regex key binding
;; ----------------------------------------------------------
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Comment/uncomment block
;; ----------------------------------------------------------
(global-set-key (kbd "C-c M-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c M-u") 'uncomment-region)

;; Eval buffer
;; ----------------------------------------------------------
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Create scratch buffer
;; ----------------------------------------------------------
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Move windows
;; ----------------------------------------------------------
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>")  'windmove-left)
(global-set-key (kbd "<s-up>")    'windmove-up)
(global-set-key (kbd "<s-down>")  'windmove-down)

;; Mark all
;; ----------------------------------------------------------
(global-set-key (kbd "C-c C-a") 'mark-whole-buffer)

;; Magit
;; ----------------------------------------------------------
(global-set-key (kbd "C-x m") 'magit-status)

;; Clever newlines
;; ----------------------------------------------------------
(global-set-key (kbd "<C-return>")   'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>")   'new-line-in-between)
(global-set-key (kbd "RET")          'newline-and-indent)

;; Line movement
;; ----------------------------------------------------------
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Fold the active region
;; ----------------------------------------------------------
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Duplicate region
;; ----------------------------------------------------------
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Yank and indent
;; ----------------------------------------------------------
(global-set-key (kbd "C-S-y") 'yank-indented)

;; Toggle quotes
;; ----------------------------------------------------------
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
;; ----------------------------------------------------------
(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
;; ----------------------------------------------------------
(global-set-key (kbd "C-+") 'change-number-at-point)

;; Buffer file functions
;; ----------------------------------------------------------
(global-set-key (kbd "C-x C-t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-d") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'kill-other-buffers)

;; Jump from file to containing directory
;; ----------------------------------------------------------
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x M-j") '(lambda () (interactive) (dired-jump 1)))

;; undo-tree
;; ----------------------------------------------------------
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; Helm
;; ----------------------------------------------------------
(global-set-key (kbd "C-c h") 'helm-prelude)
(global-set-key (kbd "M-y")   'helm-show-kill-ring)

;; Org mode
;; ----------------------------------------------------------
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; keybinding to toggle full screen mode
;; ----------------------------------------------------------
(global-set-key [(super return)] 'ns-toggle-fullscreen)

(provide 'prelude-keybindings)
