(use-package org-bullets
:ensure t
:init
(setq org-bullets-bullet-list
'("" "" "" ""))
:config
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-set-faces
'(org-level-1 ((t (:height 2.0 :foreground "#a71d31"))))
'(org-level-2 ((t (:height 1.5 :foreground "#8D6B94"))))
'(org-level-3 ((t (:height 1.25 ))))
'(org-level-4 ((t (:height 1.15 ))))
)

(use-package org-roam
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

(setq org-roam-capture-templates
'(("d" "default" plain (function org-roam-capture--get-point)
"%?"
:file-name "${slug}"
:head "#+TITLE: ${title}
#+startup: latexpreview showall

#+ROAM_ALIAS:
#+CREATED: %u

- tags ::

\* ${title}
:PROPERTIES:
:ANKI_DECK: Master
:ANKI_NOTE_TYPE: Basic
:END:
\** Front
    ${title}
\** Back
"
:unnarrowed t
:immediate-finish t)))

(defun push-anki-h()
  (when (org-roam--org-roam-file-p)
    (anki-editor-push-notes)))

(use-package company-org-roam
  :ensure t
  ;; You may want to pin in case the version from stable.melpa.org is not working 
  ; :pin melpa
  :config
  (push 'company-org-roam company-backends))

(add-hook 'after-save-hook 'push-anki-h)

(use-package org-download)
(add-hook 'dired-mode-hook 'org-download-enable)


(setq org-startup-with-inline-images t)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

(provide 'init-org)
