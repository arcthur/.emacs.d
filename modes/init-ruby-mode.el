;; Ruby
(autoload 'rhtml-mode "rhtml-mode")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

(defun ruby--jump-to-test ()
  (find-file
   (replace-regexp-in-string
    "/lib/" "/test/"
    (replace-regexp-in-string
     "/\\([^/]+\\).rb$" "/test_\\1.rb"
     (buffer-file-name)))))

(defun ruby--jump-to-lib ()
  (find-file
   (replace-regexp-in-string
    "/test/" "/lib/"
    (replace-regexp-in-string
     "/test_\\([^/]+\\).rb$" "/\\1.rb"
     (buffer-file-name)))))

(defun ruby-jump-to-other ()
  (interactive)
  (if (string-match-p "/test/" (buffer-file-name))
      (ruby--jump-to-lib)
    (ruby--jump-to-test)))

; (define-key ruby-mode-map (kbd "C-c t") 'ruby-jump-to-other)
;
;;----------------------------------------------------------------------------
;; Ruby - handy helpers
;;----------------------------------------------------------------------------

;; Borrowed from https://github.com/textmate/ruby.tmbundle/blob/master/Commands/Convert%20Ruby%20hash%20to%201_9%20syntax.tmCommand
(defun sanityinc/ruby-toggle-hash-syntax (beg end)
  "Toggle between ruby 1.8 and 1.9 hash styles."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (cond
     ((save-excursion (search-forward "=>" end t))
      (replace-regexp ":\\(\\w+\\) +=> +" "\\1: " nil beg end))
     ((save-excursion (re-search-forward "\\w+:" end t))
      (replace-regexp "\\(\\w+\\):\\( *\\(?:\"\\(?:\\\"\\|[^\"]\\)*\"\\|'\\(?:\\'\\|[^']\\)*'\\|\\w+([^)]*)\\|[^,]+\\)\\)" ":\\1 =>\\2" nil beg end)))))

(provide 'init-ruby-mode)
