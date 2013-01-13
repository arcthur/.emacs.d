(require 'flycheck)

(setq flycheck-checkers
      '(flycheck-checker-bash
        flycheck-checker-coffee
        flycheck-checker-css
        flycheck-checker-emacs-lisp
        flycheck-checker-haml
        flycheck-checker-html
        flycheck-checker-nxhtml
        flycheck-checker-json
        flycheck-checker-javascript-jshint
        flycheck-checker-javascript-jslint
        flycheck-checker-php
        flycheck-checker-python-pycheckers
        flycheck-checker-python-flake8
        flycheck-checker-python-pylint
        flycheck-checker-python-pyflakes
        flycheck-checker-ruby
        flycheck-checker-php
        flycheck-checker-sh
        flycheck-checker-tex-lacheck
        flycheck-checker-tex-chktex
        flycheck-checker-zsh
        flycheck-checker-xml-xmlstarlet
        flycheck-checker-go
        flycheck-checker-lua))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar flycheck-checker-nxhtml
  '(:command
    ("tidy" "-e" "-q" source)
    :error-patterns
    (("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)" nil 1 2 4))
    :modes nxhtml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create 'pycheckers' file, make executable and put it in PATH
;;
;; #!/bin/bash
;; pyflakes "$1"
;; pep8 --ignore=E221,E701,E202 --repeat "$1"
;; true

(defvar flycheck-checker-python-pycheckers
  '(:command ("pycheckers" source-inplace) :modes python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar flycheck-checker-xml-xmlstarlet
  '(:command
    ("xmlstarlet" "val" "-e" "-q" source-inplace)
    :modes nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar flycheck-checker-go
  '(:command
    ("go" "build" "-o" "/dev/null" source)
    :modes go-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar flycheck-checker-lua
  '(:command
    ("luac" "-p" source)
    :error-patterns
    (("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$"
      2 3 nil 4))
    :modes lua-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flycheck-enable-except-on-temp-buffers ()
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      (flycheck-mode)))

(add-hook 'nxml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'sgml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'nxhtml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'LaTeX-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'python-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'go-mode-hook 'flycheck-enable-except-on-temp-buffers)

(provide 'init-flycheck)
