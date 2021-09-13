(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")

(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))

(use-package vue-mode
  :mode "\\.vue\\'"
  :commands (vue-mode))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package instant-rename-tag
  :straight (instant-rename-tag :type git :host github :repo "manateelazycat/instant-rename-tag")
  :bind ("C-c <" . instant-rename-tag))

(use-package json-mode
  :mode "\\.json\\'")

(provide 'init-web)
