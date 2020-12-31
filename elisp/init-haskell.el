(use-package haskell-mode :diminish)

(use-package lsp-haskell :diminish)

(setq haskell-process-suggest-remove-import-lines t 
      haskell-process-auto-import-loaded-modules t)

(setq lsp-haskell-server-path "ghcide"
          lsp-haskell-server-args nil)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(provide 'init-haskell)
