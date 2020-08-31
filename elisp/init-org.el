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


(provide 'init-org)
