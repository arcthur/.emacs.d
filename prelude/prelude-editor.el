;; Basic
;; ----------------------------------------------------------
;; Don't break lines
(setq-default truncate-lines t)

;; Sentences do not need double spaces to end.
(set-default 'sentence-end-double-space nil)

;; Subword mode (consider CamelCase chunks as words)
(global-subword-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Text mode
;; ----------------------------------------------------------
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'abbrev-mode)

;; Tabs and Column defs
;; ----------------------------------------------------------
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

;; CUA selection mode
;; ----------------------------------------------------------
(cua-selection-mode t)
;; Remove text in active region if inserting text
(setq delete-selection-mode t)

;; Encoding
;; ----------------------------------------------------------
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)

;; Extensions
;; ----------------------------------------------------------
(eval-after-load 'ido '(require 'init-ido))
(eval-after-load 'shell '(require 'init-shell))
(require 'init-acejump)
(require 'init-ack)
(require 'init-autocomplete)
(require 'init-diminish)
(require 'init-desktop)
(require 'init-eproject)
(require 'init-ffip)
(require 'init-gtags)
(require 'init-hippie)
(require 'init-helm)
(require 'init-highlight)
(require 'init-hippie)
(require 'init-perspective)
(require 'init-paredit)
(require 'init-speedbar)
(require 'init-spelling)
(require 'init-smartparens)
(require 'init-undotree)
(require 'init-webjump)
(require 'init-yasnippet)

;; Whitespace style
;; ----------------------------------------------------------
(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 80)

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Parenthesis - show matching parens immediately
;; ----------------------------------------------------------
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; Dired - auto refresh dired, but be quiet about it
;; ----------------------------------------------------------
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Bookmarks
;; ----------------------------------------------------------
(setq bookmark-default-file (expand-file-name "bookmarks" prelude-savefile-dir)
      bookmark-save-flag 1)

;; Show Active Region
;; ----------------------------------------------------------
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Fix backward-up-list to understand quotes
;; ----------------------------------------------------------
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;; Enable Commands - allow commands which would be disabled by default
;; ----------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Eshell
;; ----------------------------------------------------------
(setq eshell-directory-name (expand-file-name "eshell" prelude-savefile-dir))

;; Ediff
;; ----------------------------------------------------------
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Yank indent
;; ----------------------------------------------------------
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(python-mode LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped). Only
modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

;; When popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

;; Fill column indicator
;; ----------------------------------------------------------
(defun sanityinc/fci-enabled-p ()
  (and (boundp 'fci-mode) fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;; Regenerate fci-mode line images after switching themes
(defadvice enable-theme (after recompute-fci-face activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (sanityinc/fci-enabled-p)
        (turn-on-fci-mode)))))

(provide 'prelude-editor)
