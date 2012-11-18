(require 'eproject)

(define-project-type emacs (generic)
  (look-for "init.el"))

(define-project-type vim (generic)
  (look-for "vimrc"))

(define-project-type ruby (generic)
  (or (look-for "Rakefile") (look-for "Gemfile") (look-for "config.ru")
    (look-for "\.gemspec$") (look-for "Capfile"))
      :irrelevant-files ("^[#]" ".git/" "vendor/" "coverage/" "doc/" "docs/"
                                          "\.DS_Store"))

(define-project-type node-js (generic)
  (or (look-for "package.json") (look-for "Cakefile"))
    :irrelevant-files ("^[#]" ".git/" "node_modules"))

;; eproject global bindings
(defmacro .emacs-curry (function &rest args)
  `(lambda () (interactive)
     (,function ,@args)))

(defmacro .emacs-eproject-key (key command)
  (cons 'progn
    (loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
      collect
        `(global-set-key
           (kbd ,(format "C-x p %s" k))
           (.emacs-curry ,command ,p)))))

(.emacs-eproject-key "k" eproject-kill-project-buffers)
(.emacs-eproject-key "v" eproject-revisit-project)
(.emacs-eproject-key "b" eproject-ibuffer)
(.emacs-eproject-key "o" eproject-open-all-project-files)

(provide 'init-eproject)
