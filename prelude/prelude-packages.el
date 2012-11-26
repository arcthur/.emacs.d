 ;; So the idea is that you copy/paste this code into your *scratch* buffer,
 ;; hit C-j, and you have a working developper edition of el-get.
(unless (require 'el-get nil t)
    (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
        (lambda (s) (let (el-get-master-branch) (goto-char (point-max)) (eval-print-last-sexp)))))

(setq el-get-sources
      '((:name expand-region
               :type github
               :pkgname "magnars/expand-region.el"
               :features expand-region)

        (:name change-inner
               :type github
               :pkgname "magnars/change-inner.el"
               :features change-inner
               :depends expand-region)

        (:name smart-forward
               :type github
               :pkgname "magnars/smart-forward.el"
               :features smart-forward
               :depends expand-region)

        ;; Speedbar
        (:name sr-speedbar
               :type github
               :pkgname "emacsmirror/sr-speedbar"
               :compile (".")
               :load-path (".")
               :features sr-speedbar)

        ;; Org-mode
        (:name org-mode
                :type git
                :url "git://orgmode.org/org-mode.git"
                :info "doc"
                :build/berkeley-unix `,(mapcar
                                        (lambda (target)
                                        (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                        '("clean" "all"))
                :build `,(mapcar
                        (lambda (target)
                            (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                        '("clean" "all"))
                :load-path ("." "lisp" "contrib/lisp")
                :autoloads nil
                :features org)

        (:name org-bullets
               :type github
               :pkgname "sabof/org-bullets"
               :features org-bullets)

        ;; Helm (quickly find)
        (:name helm
               :type github
               :pkgname "emacs-helm/helm"
               :features helm-config)

        (:name helm-projectile
               :type github
               :pkgname "bbatsov/projectile"
               :features projectile)

        ;; Ack
        (:name ack-and-a-half
               :type github
               :pkgname "jhelwig/ack-and-a-half"
               :features ack-and-a-half)

        ;; Highlight
        (:name volatile-highlights
               :type github
               :pkgname "k-talo/volatile-highlights.el"
               :features volatile-highlights)

        (:name highlight-indentation
               :type github
               :pkgname "antonj/Highlight-Indentation-for-Emacs"
               :features highlight-indentation)

        ;; Undo
        (:name undo-tree
               :type github
               :pkgname "emacsmirror/undo-tree"
               :features undo-tree)

        ;; Mark
        (:name dash
               :type github
               :pkgname "magnars/dash.el"
               :features "dash")

        (:name mark-multiple
               :type github
               :pkgname "magnars/mark-multiple.el"
               :features mark-multiple)

        (:name multiple-cursors
               :type github
               :pkgname "magnars/multiple-cursors.el"
               :features multiple-cursors
               :depends (mark-multiple dash))

        ;; Parens
        (:name smartparens
               :type github
               :pkgname "Fuco1/smartparens"
               :features smartparens)

        ;; Color theme
        (:name color-theme-sanityinc-tomorrow
               :type github
               :pkgname "purcell/color-theme-sanityinc-tomorrow"
               :features color-theme-sanityinc-tomorrow)

        ;; Javascript
        js3-mode
        js2-mode
        js2-refactor

        ;; Coffee mode
        (:name coffee-mode
               :type github
               :pkgname "defunkt/coffee-mode"
               :features coffee-mode
               :lazy t)

        ;; Snippets
        (:name yasnippet
              :type github
              :pkgname "capitaomorte/yasnippet"
              :features "yasnippet"
              :compile "yasnippet.el")

        ;; Auto Complete
        (:name popup
               :type github
               :pkgname "auto-complete/popup-el")

        (:name fuzzy
               :type github
               :pkgname "auto-complete/fuzzy-el")

        (:name auto-complete
               :type github
               :pkgname "auto-complete/auto-complete"
               :depends (popup fuzzy))

        auto-complete-latex

        ;; Flymake
        (:name flycheck
               :type github
               :pkgname "lunaryorn/flycheck"
               :features "flycheck")

        ;; Node jade && stylus
        (:name jade-mode
               :type github
               :pkgname "brianc/jade-mode")

        ;; Rainbow
        (:name rainbow-mode
               :type github
               :pkgname "arcthur/rainbow-mode"
               :features "rainbow-mode")

        ;; ido
        (:name ido-ubiquitous
               :type github
               :pkgname "DarwinAwardWinner/ido-ubiquitous"
               :features "ido-ubiquitous")

        (:name smex
              :type github
              :pkgname "nonsequitur/smex"
              :features smex)

        ;; Perspective
        (:name perspective
               :type github
               :pkgname "magnars/perspective-el"
               :features "perspective")

        ;; Go
        (:name go-mode
               :type http
               :url "http://go.googlecode.com/hg/misc/emacs/go-mode.el?r=tip"
               :localname "go-mode.el"
               :features go-mode)

        ;; Markdown-mode
        (:name markdown-mode
               :type git
               :url "git://jblevins.org/git/markdown-mode.git")
))

(setq prelude-packages
      (append '(
        auctex
        ace-jump-mode
        diminish
        el-get
        evil
        eproject
        find-file-in-project
        highlight-parentheses
        highlight-symbol
        inf-ruby
        jquery-doc
        magit
        magithub
        git-modes
        nyan-mode
        paredit
        rvm
        ruby-mode
        rainbow-delimiters
        s
        smart-tab
        yari
        zencoding-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync prelude-packages)

(provide 'prelude-packages)
