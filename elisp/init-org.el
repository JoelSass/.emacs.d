(setq org-agenda-files '("~/MEGA/tasks.org"))

(setq org-todo-keywords
  '((sequence
     "TODO(t!)"
     "GO(g@)"
     "WAIT(w@)"
     "BLOCKED(b@)"
     "REVIEW(r!)"
     "|"
     "DONE(d@)"
     "CANCELED(c@)"
     )))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

(setq org-roam-dailies-directory "~/MEGA/org-roam/daily/")

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

(use-package org-roam
  :diminish
:hook
(after-init . org-roam-mode)
:custom
(org-roam-directory "~/MEGA/org-roam/")
:bind (:map org-roam-mode-map
(("C-c n l" . org-roam)
 ("C-c n f" . org-roam-find-file)
 ("C-c n g" . org-roam-graph))
:map org-mode-map
(("C-c n i" . org-roam-insert))
(("C-c n I" . org-roam-insert-immediate))))

(setq org-roam-dailies-capture-templates
  `(("d" "default" entry (function org-roam-capture--get-point)
     "* %?"
     :file-name ,(concat org-roam-dailies-directory "%<%Y-%m-%d>")
     :head "#+title: %<%Y-%m-%d>\n
[[file:../daily.org][Daily]]
* Diary
\n** Heute erreicht")))

(setq org-roam-capture-templates
'(("d" "default" plain (function org-roam-capture--get-point)
"%?"
:file-name "${slug}"
:head "#+TITLE: ${title}
#+startup: latexpreview showall

#+ROAM_ALIAS:
#+CREATED: %u

#+roam_tags: University ADS
\n* ${title}
* Siehe Auch
* Quellen
"
:unnarrowed t
:immediate-finish t)))

(add-hook 'after-save-hook 'push-anki-h)

(defun push-anki-h()
  (when (org-roam--org-roam-file-p)
    (anki-editor-push-notes)))

(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

(use-package org-roam-server
  :diminish
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8078
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(setq org-roam-graph-node-extra-config
        '(("shape"      . "underline")
          ("style"      . "rounded,filled")
          ("fillcolor"  . "#EEEEEE")
          ("color"      . "#C9C9C9")
          ("fontcolor"  . "#111111")
          ("fontname"   . "Overpass")))

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


(use-package doct :diminish)

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
				   :file "~/MEGA/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
				   :file "~/MEGA/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a")
                   )
                  ("University" :keys "u"
                   :icon ("graduation-cap" :set "faicon" :color "purple")
				   :file "~/MEGA/tasks.org"
                   :headline "University"
                   :unit-prompt ,(format "%%^{Unit|%s}" (string-join (read-lines "~/MEGA/uni-units") "|"))
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
		   :file "~/MEGA/tasks.org"
		   :prepend t
		   :type entry
		   :children (("Simple" :keys "s"
			       :icon ("create" :set "material" :color "green")
			       :template ("* Item \t :drill: \n %^{Question} ? \n** The Answer \n %^{Answer} ."))
			      ("Cloze" :keys "c"
			       :icon ("more_horiz" :set "material" :color "green")
			       :template ("* Item \t :drill: \n %^{sentence} [ %^{placeholder} ]"))
			      ("Double-sided" :keys "d"
			       :icons ("call_split" :set "material" :color "green")
			       :template ("* Item \t :drill: \n \t :PROPERTIES: \n \t :DRILL_CARD_TYPE: twosided \n \t :END: \n \n %^{Question} \n \n ** %^{First side} \n %^{First side answer} \n \n ** %^{second side} \n %^{second side answer}"))))
		  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
				   :file "~/MEGA/tasks.org"
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
				   :file "~/MEGA/tasks.org"
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
				   :file "~/MEGA/tasks.org"
                   :prepend t
                   :headline "Tasks"
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

(define-key global-map (kbd "C-c X")
  (lambda () (interactive) (org-capture)))

(defun +org-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg "o"))

(define-key global-map (kbd "C-c x")
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


(provide 'init-org)
