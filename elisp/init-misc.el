(use-package elcord
  :config
  (elcord-mode 1))

(defun +org-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg "a")
  (get-buffer org-agenda-buffer-name))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/Dropbox/e_z_money.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 4)))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
      `(
        ((,(all-the-icons-material "today" :height 0.9 :v-adjust 0.0)
         "Daily"
         "Go to the daily org-roam file"
         (lambda (&rest _) (org-roam-dailies-goto-today)))
		 (,(all-the-icons-fileicon "brain" :height 1.0 :v-adjust 0.0)
		  "Flashcards"
		  "Start org drill"
		  (lambda (&rest _) (org-drill org-agenda-files)))	 
	 (,(all-the-icons-octicon "checklist" :height 1.0 :v-adjust 0.0)
         "Agenda"
         "Go to the daily agenda"
         (lambda (&rest _) (+org-agenda)))
	 (,(all-the-icons-octicon "mail" :height 1.0 :v-adjust 0.0)
	  "Mail"
	  "Open mu to read mail"
	  (lambda (&rest _) (mu4e)))
	 ))))


(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

(provide 'init-misc)
