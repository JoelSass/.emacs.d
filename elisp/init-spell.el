(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "de_DE.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "de_DE,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_US")
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

(setq flyspell-sort-corrections nil)
(setq flyspell-issue-message-flag nil)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(provide 'init-spell)
