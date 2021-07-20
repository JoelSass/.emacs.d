(use-package ivy
  :load-path (lambda () (expand-file-name "packages/swiper" user-emacs-directory))
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :diminish
  :load-path (lambda () (expand-file-name "packages/ivy-rich" user-emacs-directory))
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :load-path (lambda () (expand-file-name "packages/swiper" user-emacs-directory))
  :diminish
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :load-path (lambda () (expand-file-name "packages/prescient.el" user-emacs-directory))
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package company-prescient
  :load-path (lambda () (expand-file-name "packages/prescient.el" user-emacs-directory))
  :after company
  :config
  (company-prescient-mode 1))

(prescient-persist-mode 1)

(provide 'init-search)
