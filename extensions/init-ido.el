;; Interactively Do Things

(setq ido-auto-merge-work-directories-length nil
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-enable-last-directory-history t
      ido-everywhere t
      ido-ignore-extensions t
      ido-case-fold nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir)
      ido-default-file-method 'selected-window)
(ido-mode t)

;; Ignore the .aux extensions that TeX programs create
(setq completion-ignored-extensions
      (cons "*.aux" completion-ignored-extensions))

(add-hook
 'ido-setup-hook
 #'(lambda ()
     ;; Use C-w to go back up a dir to better match normal usage of C-w
     ;; - insert current file name with C-x C-w instead.
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
     (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; auto-completion in minibuffer
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-files "\\.DS_Store")
(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Display ido results vertically, rather than horizontally
(setq ido-decorations '("\n   " "" "\n   " "\n   ..." "{" "}" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Allow spaces when using ido-find-file
(add-hook 'ido-make-file-list-hook
    (lambda ()
        (define-key ido-file-dir-completion-map (kbd "SPC") 'self-insert-command)))

;; Use ido everywhere
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)

;; smex
(smex-initialize)

(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

(defun beautify-smex ()
  (interactive)
  (unwind-protect (progn (setq ido-decorations '( "{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
                         (smex))
    (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))))

(provide 'init-ido)
