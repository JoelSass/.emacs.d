(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Welcome to the Church of Emacs")
  (setq dashboard-startup-banner "~/MEGA/e_z_money.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 10)
                        (agenda . 5)))
  (setq show-week-agenda-p t)
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
