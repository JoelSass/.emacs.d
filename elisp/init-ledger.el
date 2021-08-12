(use-package ledger-mode
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :bind (:map ledger-mode-map
	("C-x C-s" . my/ledger-save))
  :preface
  (defun my/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (save-excursion
      (when (buffer-modified-p)
        (with-demoted-errors (ledger-mode-clean-buffer))
        (save-buffer)))))

(use-package company-ledger
  :after ledger-mode)

(use-package evil-ledger
  :after ledger-mode)

(provide 'init-ledger)
