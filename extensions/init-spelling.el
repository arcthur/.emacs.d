;;; spell configuration
;; flyspell-mode does spell-checking on the fly as you type
(setq ispell-dictionary "en"
      ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(defadvice ispell-command-loop (after emagician/fix-muscle-memory last activate)
  "Force the user to type in the misspelled/mis-typoed word 5 times, to burn it into muscle memory."
  (let ((times 0)
        (total-times 3))
    (while (< times total-times)
      (setq times
            (+ times (if (string= (read-string (format "Re-type \"%s\" correctly (%d/%d): "  ad-return-value times total-times))
                              ad-return-value)
                     1
                   -1))))))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'init-spelling)
