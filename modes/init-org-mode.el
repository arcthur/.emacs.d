(require 'org)
(setq org-modules '(org-bbdb
                    org-contacts
                    org-info
                    org-jsinfo
                    org-habit
                    org-irc
                    org-mouse
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                    org-man
                    org-panel
                    org-screen
                    org-toc))

;; Various preferences
(setq org-completion-use-ido t)
(setq org-edit-timestamp-down-means-later t)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-export-kill-product-buffer-when-displayed t)
(setq org-icalendar-include-todo t)

; (define-key global-map "\C-cL" 'org-insert-link-global)
; (define-key global-map "\C-cO" 'org-open-at-point-global)
; (define-key global-map "\C-cv" 'org-show-todo-tree)

; (define-key org-mode-map "C-c C-r" 'org-refile)
; (define-key org-mode-map "C-c R" 'org-reveal)
; (define-key org-mode-map "C-M-w" 'append-next-kill)
;
; (define-key org-agenda-mode-map "i" 'org-agenda-clock-in)

;; -----------------------------------------------------------
;; Taking notes
;; -----------------------------------------------------------
(setq org-directory "~/Dropbox/personal")
(setq org-default-notes-file (concat org-directory "/organizer.org"))

;;; Templates
(setq org-capture-templates
      '(("t" "Tasks" entry
         (file+headline "~/Dropbox/personal/organizer.org" "Tasks")
         "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("q" "Quick task" entry
         (file+headline "~/Dropbox/personal/organizer.org" "Tasks")
         "* TODO %^{Task}
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:"
         :immediate-finish t)
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/Dropbox/personal/ledger")
         "%(org-read-date) %^{Payee}
  Liabilities:MBNA
  Expenses:%^{Account}  $%^{Amount}
" :immediate-finish)
        ("ln" "No Frills" plain
         (file "~/Dropbox/personal/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
  Liabilities:MBNA
  Assets:Wayne:Groceries  $%^{Amount}
" :immediate-finish)
        ("lc" "Cash" plain
         (file "~/Dropbox/personal/ledger")
         "%(org-read-date) * %^{Payee}
  Expenses:Cash
  Expenses:%^{Account}  %^{Amount}
")
        ("n" "Tracking" table-line
         (file+headline "~/Dropbox/personal/organizer.org" "Tracking")
         "| %t | %^{Woke up} | %(org-count-words-today) | %^{Slept} |")
        ("b" "Book" entry
         (file+datetree "~/Dropbox/personal/books.org" "Inbox")
         "* %^{Title}  %^g
%i
*Author(s):* %^{Author} \\\\
*ISBN:* %^{ISBN}

%?

*Review on:* %^t \\
%a
%U"
         :clock-in :clock-resume)
         ("c" "Contact" entry (file "~/Dropbox/personal/contacts.org")
          "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
         ("r" "Notes" entry
          (file+datetree "~/Dropbox/personal/organizer.org" "Inbox")
          "* %?\n\n%i\n"
          :clock-in :clock-resume)))
; (define-key global-map "\C-M-r" 'org-capture)

;;; Refiling
; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; When I use org-refile to organize my notes, I like seeing the latest entries on top.
(setq org-reverse-note-order t)
(setq org-refile-targets
      '(("~/Dropbox/personal/contacts.org" . (:maxlevel . 2))
        ("~/Dropbox/personal/decisions.org" . (:maxlevel . 3))
        ("~/Dropbox/personal/outline.org" . (:maxlevel . 10))
        ("~/Dropbox/personal/organizer.org" . (:maxlevel . 4))))
(setq org-blank-before-new-entry nil)

;;; Clock
(require 'org-clock)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(defun org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
             (minutes (org-clock-sum-current-item))
             (wpm (/ words minutes)))
        (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
        (kill-new (number-to-string wpm))))))

;; -----------------------------------------------------------
;; Managing tasks
;; -----------------------------------------------------------
(setq org-todo-keywords
 '((sequence
    "TODO(t)"
    "STARTED(s)"
    "WAITING(w@/!)"
    "POSTPONED(p)" "SOMEDAY(s@/!)" "|" "DONE(x!)" "CANCELLED(c@)")
   (sequence "TODELEGATE(-)" "DELEGATED(d)" "COMPLETE(x)")))

;;; Projects
(setq org-tags-exclude-from-inheritance '("PROJECT"))

;;; Tasks
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@errands" . ?e)
                      ("quantified" . ?q)))

;;; Enable filtering by effort estimates
(setq org-global-properties
      '(("Effort_ALL". "0 0:10 0:30 1:00 2:00 3:00 4:00")))

;;; Track time
(setq org-clock-idle-time nil)
(setq org-log-done 'time)
(defadvice org-clock-in (after wicked activate)
"Mark STARTED when clocked in"
(save-excursion
  (catch 'exit
    (org-back-to-heading t)
    (if (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
    (if (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
        (org-todo "STARTED")))))

;;; Habits
(setq org-habit-graph-column 80)

;; -----------------------------------------------------------
;; Agenda
;; -----------------------------------------------------------
(setq org-agenda-files '("~/Dropbox/personal/organizer.org"
                         "~/Dropbox/personal/business.org"
                         "~/Dropbox/personal/food.org"
                         "~/Dropbox/personal/routines.org"))

(setq org-agenda-span 2)
(setq org-agenda-include-diary t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-agenda-custom-commands
      '(("W" todo "WAITING")
        ("T" todo-tree "TODO")
        ("t" todo ""
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'org-sort-agenda-items-todo)
          (org-agenda-view-columns-initially t)
          ))
        ;; Weekly review
        ("w" "Weekly review" agenda "" ((org-agenda-span 7) (org-agenda-log-mode 1)))
        ("1" tags-todo "+@work"
         ((org-agenda-view-columns-initially t)
          (org-agenda-cmp-user-defined 'org-sort-agenda-items-todo)))
        ("2" tags-todo "+@home" ((org-agenda-view-columns-initially t)))
        ("3" tags-todo "+@phone" ((org-agenda-view-columns-initially
                                   t)))
        ("7" "Timeline" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))
        ("w" todo "WAITING")
        ("u" "Unscheduled" ((org-agenda-list-unscheduled)))
        ("v" tags-todo "+BOSS-URGENT")
        ("U" tags-tree "+BOSS-URGENT")
        ("f" occur-tree "\\<FIXME\\>")
        ("r" occur-tree ":rough:")
        ("b" occur-tree ":toblog:")
        ("p" tags "+PROJECT")
        ))

;;; Sorting by date and priority
(setq org-agenda-sorting-strategy
      '((agenda time-up user-defined-up todo-state-up priority-down effort-up)
        (todo user-defined-up todo-state-up priority-down effort-down)
        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'org-sort-agenda-items-user-defined)
(require 'cl)
(defun org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (org-compare-dates
      (org-min-date sched-a deadline-a)
      (org-min-date sched-b deadline-b)))))

(defun org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (org-complete-cmp a b)
   (org-date-cmp a b)))

(defun org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (org-get-context a))
        (context-b (org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (org-complete-cmp a b)
   (org-context-cmp a b)
   (org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

;;; Preventing things from falling through the cracksc
(defun org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

;; -----------------------------------------------------------
;; Tree
;; -----------------------------------------------------------
;; Viewing, navigating, and editing the Org tree
(setq org-cycle-include-plain-lists nil)

;; I often cut and paste subtrees. This makes it easier to cut something and paste it elsewhere in the hierarchy.
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c k") 'org-cut-subtree)
     (setq org-yank-adjusted-subtrees t)))

;; -----------------------------------------------------------
;; Publishing
;; -----------------------------------------------------------
(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)

(setq org-publish-project-alist
      '(("public"
         :base-directory "~/Dropbox/public"
         :base-extension "org"
         :publishing-directory "~/Dropbox/public"
         :publishing-function org-publish-org-to-html
         )))

;; -----------------------------------------------------------
;; Latex
;; -----------------------------------------------------------
(require 'org-latex)
(require 'org-export-latex)

;; 'djcb-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(add-to-list 'org-export-latex-classes
  '("djcb-org-article"
"\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Courier New}
\\setromanfont [BoldFont={Courier New Bold},
                ItalicFont={Courier New Italic}]{Courier New}
\\setsansfont{Gill Sans}
\\setmonofont[Scale=0.8]{Monaco}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-to-pdf-process
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes

;; org-bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(provide 'init-org-mode)
