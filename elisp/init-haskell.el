(use-package haskell-mode)

(use-package lsp-haskell)

(setq haskell-process-suggest-remove-import-lines t 
      haskell-process-auto-import-loaded-modules t)

(provide 'init-haskell)
