;; wrap-region-mode (plugin)
(wrap-region-mode t)

;; Only wrap region if trigger key is given a negative prefix argument
(setq wrap-region-only-with-negative-prefix t)

;; Don't screw up key bindings in magit-mode
(add-to-list 'wrap-region-except-modes 'magit-status-mode)

;; Custom wrap
(wrap-region-add-wrappers
 '(("$" "$")
   ("{-" "-}" "#")
   ("/" "/" nil 'ruby-mode)
   ("/* " " */" "#" '(js2-mode css-mode))
   ("{ value: " " }" "v" 'js2-mode)
   ("$(" ")" "$" 'js2-mode)
   ("`" "`" nil '(markdown-mode ruby-mode))
   ("*" "*" "*" 'markdown-mode)
   ("$" "$" nil 'latex-mode)
   ("``" "''" "\"" 'latex-mode)
   ("<p>" "</p>" "p" 'html-mode)
   ("<div>" "</div>" "d" 'html-mode)
   ("<li>" "</li>" "l" 'html-mode)
   ("<strong>" "</strong>" "s" 'html-mode)
   ("<a href=\"\">" "</a>" "a" 'html-mode)
   ("<span class=\"required\">" "</span>" "r" 'html-mode)
   ("<h1>" "</h1>" "h" 'html-mode)
   ("${" "}" "$" 'html-mode)
))

(provide 'init-wrap-region)
