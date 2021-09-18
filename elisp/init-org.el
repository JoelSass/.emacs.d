(setq org-agenda-files '("~/Dropbox/org"))

(use-package org-drill
  :config (progn
			(add-to-list 'org-modules 'org-drill)
			(setq org-drill-add-random-noise-to-intervals-p t)
			(setq org-drill-hint-separator "||")
			(setq org-drill-learn-fraction 0.25)
			))

(setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq-default
   ;; Different colors for different priority levels
   org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline))
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-skip-unavailable-files t
   ;; Shift the agenda to show the previous 3 days and the next 7 days for
   ;; better context on your week. The past is less important than the future.
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d")

 (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

(setq org-roam-dailies-directory "~/Dropbox/org/roam/daily/")

(setq org-image-actual-width '(600))

(use-package org-superstar
:diminish
:init
(setq org-superstar-headline-bullets-list
'("" "" "" "" "" ""))
:config
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(custom-set-faces
'(org-level-1 ((t (:height 2.0 :foreground "#a71d31"))))
'(org-level-2 ((t (:height 1.5 :foreground "#8D6B94"))))
'(org-level-3 ((t (:height 1.25 ))))
'(org-level-4 ((t (:height 1.15 ))))
'(org-level-5 ((t (:height 1.05 ))))
)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory (file-truename "/home/memento/Dropbox/org/roam"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
			 ("C-c n a" . org-roam-alias-add)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-db-autosync-mode)
      (require 'org-roam-protocol))

(setq org-roam-v2-ack t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")

(setq org-roam-dailies-capture-templates
  `(("d" "default" entry "* %?" :if-new
     (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: Daily\n* Tagebuch\n** Heute Erreicht
     "))))

(setq org-roam-capture-templates
  `(("d" "default" plain "#+startup: latexpreview showall
#+CREATED: %u
#+filetags: TODO
\n* ${title}
%?
* Siehe Auch
* Quellen
* Footnote" :if-new
     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}\n")
	 :unnarrowed t)
	("p" "People" plain "#+filetags: :TODO:People:
* ${title}
%?
** Leben
** Werk" :if-new
	 (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}\n")
	 :unnarrowed t)
		("f" "Bekannte" plain "#+filetags: :TODO:People:
* ${title}
%?
** Geburtstag
** Mag gern" :if-new
	 (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}\n")
	 :unnarrowed t
	)))


(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

(setq org-startup-indented t)
(add-hook 'after-save-hook 'push-anki-h)

(defun push-anki-h()
  (when (string-equal major-mode "org-mode")
    (anki-editor-push-notes)))

(use-package org-super-agenda
  :diminish
  :commands (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))


(use-package doct
  :diminish)

(defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (message "-------")
                                     (pp template)
                                     (message "-- %d" (length template))
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (message "--icon found")
                                       (pp spec)
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


(setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
				   :file "~/Dropbox/org/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
				   :file "~/Dropbox/org/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a")
                   )
                  ("University" :keys "u"
                   :icon ("graduation-cap" :set "faicon" :color "purple")
				   :file "~/Dropbox/org/tasks.org"
                   :headline "University"
                   :unit-prompt ,(format "%%^{Unit|%s}" (string-join (read-lines "~/Dropbox/uni-units") "|"))
                   :prepend t
                   :type entry
                   :children (("Test" :keys "t"
                               :icon ("timer" :set "material" :color "red")
                               :template ("* TODO [#C] %{unit-prompt} %? :uni:tests:"
                                          "SCHEDULED: %^{Test date:}T"
                                          "%i %a"))
                              ("Assignment" :keys "a"
                               :icon ("library_books" :set "material" :color "orange")
                               :template ("* TODO [#B] %{unit-prompt} %? :uni:assignments:"
                                          "DEADLINE: %^{Due date:}T"
                                          "%i %a"))
                              ("Miscellaneous task" :keys "u"
                               :icon ("list" :set "faicon" :color "yellow")
                               :template ("* TODO [#D] %{unit-prompt} %? :uni:"
                                          "%i %a"))))
	  ("Drill" :keys "d"
		   :icon ("brain" :set "fileicon" :color "pink")
		   :file "~/Dropbox/org/tasks.org"
           :headline "Drill"
		   :prepend t
		   :type entry
		   :children (("Simple" :keys "s"
			       :icon ("create" :set "material" :color "green")
			       :template ("* Item\t:drill:\n%?\n** The Answer\n"))
			      ("Cloze" :keys "c"
			       :icon ("more_horiz" :set "material" :color "green")
			       :template ("* Item \t :drill:\n %? []"))
			      ("Double-sided" :keys "d"
			       :icons ("call_split" :set "material" :color "green")
			       :template ("* Item \t :drill:\n\t:PROPERTIES:\n\t:DRILL_CARD_TYPE: twosided\n\t:END:\n\n%?\n\n** \n\n\n** \n"))))
		  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
				   :file "~/Dropbox/org/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
				   :file "~/Dropbox/org/tasks.org"
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
							  ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:research"
                               )
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info"
                               )
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea"
                               )))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
				   :file "~/Dropbox/org/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra ""
                               )
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
		  )))

(define-key global-map (kbd "C-c x")
  (lambda () (interactive) (org-capture)))

(defun +org-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg "o"))

(define-key global-map (kbd "C-c X")
  (lambda () (interactive) (+org-agenda)))


(use-package org-pretty-tags
  :diminish 
   :config
    (setq org-pretty-tags-surrogate-strings
          `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
            ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
            ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
            ("drill"      . ,(all-the-icons-fileicon "brain"          :face 'all-the-icons-orange  :v-adjust 0.01))
            ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
            ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
            ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
            ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
            ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
            ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
            ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
            ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
            ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("private"    . ,(all-the-icons-octicon  "lock"           :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("birthday"   . ,(all-the-icons-faicon   "birthday-cake"  :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
    (org-pretty-tags-global-mode))

(setq org-tag-alist
    '((:startgroup)
       (:endgroup)
       ("uni")
       ("ucc")
       ("assignment")
       ("drill")
       ("test")
       ("lecture")
       ("email")
       ("web")
       ("info")
       ("issue")
       ("someday")
       ("idea")
       ("emacs")
       ("private")
       ("birthday")
       ("article")))

(use-package org-download)


(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
(setq org-habit-show-all-today t)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(defadvice org-agenda (around split-vertically activate)
  (let (
    (split-width-threshold 20)    ; or whatever width makes sense for you
    (split-height-threshold nil)) ; but never horizontally
ad-do-it))

(use-package org-pomodoro)

(alert-add-rule :category "org-pomodoro"
                :style (cond (alert-growl-command
                              'growl)
                             (alert-notifier-command
                              'notifier)
                             (alert-libnotify-command
                              'libnotify)
                             (alert-default-style)))

(use-package org-alert
  :custom (alert-default-style 'notifications)
  :config
  (setq org-alert-interval 3000
		org-alert-notification-title "Org Tasks Reminder")
  (org-alert-enable))

(use-package org-fancy-priorities)

(use-package org-noter)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{xcolor}

\\usepackage[T1]{fontenc}

\\usepackage{booktabs}

\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
")


(provide 'init-org)
