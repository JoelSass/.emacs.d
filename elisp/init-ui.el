(show-paren-mode t)
(setq show-paren-style 'expression)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time-mode 1)
(display-battery-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(use-package doom-themes
:custom-face
(cursor ((t (:background "dark goldenrod"))))
:config
(doom-themes-visual-bell-config)
(doom-themes-org-config)
(load-theme 'doom-one t))


(use-package doom-modeline
:custom
(inhibit-compacting-font-caches t)
(doom-modeline-minor-modes t)
(doom-modeline-icon t)
(doom-modeline-major-mode-color-icon t)
(doom-modeline-height 15)
:config
(doom-modeline-mode))

(provide 'init-ui)
