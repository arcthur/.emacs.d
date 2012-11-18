;; desktop settings for session management
;; currently, we only save sunrise tabs
(require 'desktop)
(defun my-desktop-filter (buffer)
  (let ((mode (with-current-buffer buffer major-mode)))
    (if (eq mode 'sr-mode) t nil)))

(setq desktop-dirname (expand-file-name "desktop" prelude-savefile-dir))
(setq desktop-path (list desktop-dirname))
(desktop-save-mode 1)
(setq history-length 250)

;; Autosave
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

(defcustom desktop-buffer-filter nil
  "If not nil, run the specified function on each buffer to determine
if it should be saved or not. The function should have type buffer -> bool.
This is a more general way to filter buffers that are to be saved than simple
blacklist."
  :type 'symbol
  :group 'desktop
  :version "24.2")

;; Modification of desktop-save to include buffer-filtering.
;; If `desktop-buffer-filter' is not nil, run it on each buffer in
;; (buffer-list) to determine if it should be saved
(defun desktop-save (dirname &optional release)
  "Save the desktop in a desktop file.
Parameter DIRNAME specifies where to save the desktop file.
Optional parameter RELEASE says whether we're done with this desktop.
See also `desktop-base-file-name'."
  (interactive "DDirectory to save desktop file in: ")
  (setq desktop-dirname (file-name-as-directory (expand-file-name dirname)))
  (save-excursion
    (let ((eager desktop-restore-eager)
          (new-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
      (when
          (or (not new-modtime)      ; nothing to overwrite
              (equal desktop-file-modtime new-modtime)
              (yes-or-no-p (if desktop-file-modtime
                               (if (> (float-time new-modtime) (float-time desktop-file-modtime))
                                   "Desktop file is more recent than the one loaded.  Save anyway? "
                                 "Desktop file isn't the one loaded.  Overwrite it? ")
                             "Current desktop was not loaded from a file.  Overwrite this desktop file? "))
              (unless release (error "Desktop file conflict")))

        ;; If we're done with it, release the lock.
        ;; Otherwise, claim it if it's unclaimed or if we created it.
        (if release
            (desktop-release-lock)
          (unless (and new-modtime (desktop-owner)) (desktop-claim-lock)))

        (with-temp-buffer
          (insert
           ";; -*- mode: emacs-lisp; coding: emacs-mule; -*-\n"
           desktop-header
           ";; Created " (current-time-string) "\n"
           ";; Desktop file format version " desktop-file-version "\n"
           ";; Emacs version " emacs-version "\n")
          (save-excursion (run-hooks 'desktop-save-hook))
          (goto-char (point-max))
          (insert "\n;; Global section:\n")
          (mapc (function desktop-outvar) desktop-globals-to-save)
          (when (memq 'kill-ring desktop-globals-to-save)
            (insert
             "(setq kill-ring-yank-pointer (nthcdr "
             (int-to-string (- (length kill-ring) (length kill-ring-yank-pointer)))
             " kill-ring))\n"))

          (insert "\n;; Buffer section -- buffers listed in same order as in buffer list:\n")
          (dolist (l (mapcar 'desktop-buffer-info (buffer-list)))
            (let ((base (pop l)))
              (when (and (apply 'desktop-save-buffer-p l)
                         (if desktop-buffer-filter (funcall desktop-buffer-filter
                                                            (get-buffer (cadr l))) t))
                (insert "("
                        (if (or (not (integerp eager))
                                (if (zerop eager)
                                    nil
                                  (setq eager (1- eager))))
                            "desktop-create-buffer"
                          "desktop-append-buffer-args")
                        " "
                        desktop-file-version)
                ;; If there's a non-empty base name, we save it instead of the buffer name
                (when (and base (not (string= base "")))
                  (setcar (nthcdr 1 l) base))
                (dolist (e l)
                  (insert "\n  " (desktop-value-to-string e)))
                (insert ")\n\n"))))

          (setq default-directory desktop-dirname)
          (let ((coding-system-for-write 'emacs-mule))
            (write-region (point-min) (point-max) (desktop-full-file-name) nil 'nomessage))
          ;; We remember when it was modified (which is presumably just now).
          (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))))))

(provide 'init-desktop)
