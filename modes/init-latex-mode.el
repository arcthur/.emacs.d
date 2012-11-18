(autoload 'LaTeX-preview-setup "preview")
(add-hook 'LaTeX-mode-hook #'LaTeX-install-toolbar)
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)

;; make AUCTEX aware of style files and multi-file documents right away
(setq TeX-auto-save t           ;; Enable parse on load
      TeX-parse-self t          ;; Enable parse on save.
      TeX-save-query nil
      TeX-auto-untabify t       ;; Don't use tab indent
      TeX-electric-escape t
      reftex-plug-into-AUCTeX t)

(setq-default TeX-master nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex -synctex=1")
 '(TeX-command-list
   (quote (
("XeLaTeX_SyncteX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX")
("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX")
("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output")
("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output")
("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX")
("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once")
("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion")
("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")
("Print" "%p" TeX-run-command t t :help "Print the file")
("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness")
("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
("Other" "" TeX-run-command t t :help "Run an arbitrary command")
("Jump to PDF" "%V" TeX-run-discard-or-function nil t :help "Run Viewer"))))
 '(TeX-modes (quote (tex-mode plain-tex-mode texinfo-mode latex-mode doctex-mode)))
 '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b") ("Preview" "open -a Preview.app %o"))))
 '(TeX-view-program-selection (cond ((eq system-type (quote windows-nt)) (quote (((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap") (output-pdf "start") (output-html "start")))) (t (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))))
 '(truncate-partial-width-windows nil))

(setq TeX-source-correlate-method 'synctex)

(provide 'init-latex-mode)
