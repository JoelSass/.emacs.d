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
  (setq dashboard-startup-banner "~/MEGA/e_z_money.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 4)))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
      `(
        ((,(all-the-icons-material "today" :height 0.9 :v-adjust 0.0)
         "Daily"
         "Go to the daily org-roam file"
         (lambda (&rest _) (org-roam-dailies-find-today)))
	 (,(all-the-icons-octicon "checklist" :height 1.0 :v-adjust 0.0)
         "Agenda"
         "Go to the daily agenda"
         (lambda (&rest _) (+org-agenda)))))))


(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

(provide 'init-misc)
