(eval-when-compile (require 'cl))

;; projectile is a project management mode
(projectile-global-mode)
(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(setq projectile-enable-caching t)

(require 'helm-config)
(require 'helm-misc)
(require 'helm-projectile)
(require 'helm-mode)
(require 'helm-match-plugin)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-locate)

(defun helm-prelude ()
  "Preconfigured `helm'."
  (interactive)
  (condition-case nil
    (if (projectile-project-root)
        ;; add project files and buffers when in project
        (helm-other-buffer '(helm-c-source-projectile-files-list
                             helm-c-source-projectile-buffers-list
                             helm-c-source-buffers-list
                             helm-c-source-buffer-not-found
                             helm-c-source-recentf
                             helm-c-source-ffap-line
                             helm-c-source-ffap-guesser
                             helm-c-source-files-in-current-dir
                             helm-c-source-file-cache
                             helm-c-source-file-name-history
                             helm-c-source-bookmarks
                             helm-c-source-buffer-not-found)
                           "*helm prelude*")
      ;; otherwise fallback to helm-mini
      (helm-mini))
    ;; fall back to helm mini if an error occurs (usually in projectile-project-root)
    (error (helm-mini))))

(setq helm-enable-shortcuts 'prefix)
(define-key helm-map (kbd "M-s") 'helm-select-with-prefix-shortcut)

;;; Shortcuts
(global-set-key (kbd "M-X") 'helm-at-point)

;; Customization
(setq helm-input-idle-delay 0)
(setq helm-idle-delay 0.1)
(setq helm-quick-update t)

;;; Bindings
(defun helm-insert-buffer-base-name ()
  "Insert buffer name stub."
  (interactive)
  (helm-insert-string
   (with-current-buffer helm-current-buffer
     (buffer-stub-name))))

;; 1. Quote the string
;; 2. If we didn't input any typically regexp characters, convert spaces to .*,
;;    however, it is still order related.
(defun helm-pattern-to-regexp (string)
  (prin1-to-string
   (if (string-match-p "[\\[\\]*+$^]" string) string
     (let ((parts (split-string string "[ \t]+" t)))
       (if (eq 2 (length parts))
           ;; for two parts a,b we make a.*b\|b.*a
           (concat
            (mapconcat 'regexp-quote parts ".*")
            "\\|"
            (mapconcat 'regexp-quote (reverse parts) ".*"))
         ;; only 1 part or more than 2 parts, fine, just combine them using .*,
         ;; thus it will slow down locate a lot. This means you have to type in order
         (mapconcat 'regexp-quote parts ".*"))))))

;; Hack
;; Convert helm pattern to regexp for locate
(defadvice helm-c-locate-init (around helm-pattern-to-regexp () activate)
  (let ((helm-pattern (helm-pattern-to-regexp helm-pattern)))
    ad-do-it))

(provide 'init-helm)
