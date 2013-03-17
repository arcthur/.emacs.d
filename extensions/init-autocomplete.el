;; autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(setq ac-comphist-file (expand-file-name "ac-comphist.dat" prelude-savefile-dir))
(ac-config-default)

(custom-set-variables
 '(ac-dwim nil)
 '(ac-auto-start nil))

; (define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)

;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
(setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
    (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(setq ac-delay 0.1
      ac-auto-show-menu 1.6
      ac-quick-help-delay 2.5
      ac-ignore-case nil
      ac-limit 20)

(set-default 'ac-sources
              '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode espresso-mode haskell-mode html-mode
                nxml-mode sh-mode smarty-mode lisp-mode textile-mode
                markdown-mode js2-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))

;; Yasnippets, always
(eval-after-load "yasnippet"
  '(setq-default ac-sources (append '(ac-source-yasnippet) ac-sources)))

;; Exclude very large buffers from dabbrev
(defun dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'dabbrev-friend-buffer)

;; Use tab for autocomplete.
(global-smart-tab-mode t)

(provide 'init-autocomplete)
