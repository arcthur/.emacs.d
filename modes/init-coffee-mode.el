;; Coffeescript
(require 'coffee-mode)

(autoload 'coffee-mode "coffee-mode" nil t)

(add-hook 'coffee-mode-hook
          (lambda ()
            (define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer)
            (define-key coffee-mode-map (kbd "M-R") 'coffee-compile-region)
            (define-key coffee-mode-map (kbd "<tab>") 'coffee-indent)
            (define-key coffee-mode-map (kbd "<backtab>") 'coffee-unindent)))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))

;; Use js2-mode for displaying compiled CS
(setq coffee-js-mode 'js2-mode)

;; Patch coffee-mode so coffee-compile-region pops up a new
;; non-focused window instead of replacing the current buffer.
(eval-after-load "coffee-mode"
  '(defun coffee-compile-region (start end)
     "Compiles a region and displays the JS in another buffer."
     (interactive "r")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (when buffer (kill-buffer buffer)))
     (call-process-region start end coffee-command nil
                          (get-buffer-create coffee-compiled-buffer-name) nil "-s" "-p" "--bare")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (with-current-buffer buffer
         (funcall coffee-js-mode)
         (goto-char (point-min)))
       (display-buffer buffer))))

;; Handle backtabs and indenting regions
(defun coffee-indent-block ()
  (shift-region coffee-tab-width)
  (setq deactivate-mark nil))

(defun coffee-unindent-block ()
  (shift-region (- coffee-tab-width))
  (setq deactivate-mark nil))

(defun coffee-indent ()
  (interactive)
  (if (and (boundp 'ac-trigger-command-p) (ac-trigger-command-p last-command))
      (auto-complete)
    (if mark-active
        (coffee-indent-block)
      (indent-for-tab-command))))

(defun coffee-unindent ()
  (interactive)
  (if mark-active
      (coffee-unindent-block)
    (progn
      (indent-line-to (- (current-indentation) coffee-tab-width)))))

(provide 'init-coffee-mode)
