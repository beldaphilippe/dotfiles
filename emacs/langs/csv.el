(use-package csv-mode
  :ensure t
  :defer t
  :hook (csv-mode . (lambda () (setq-local require-final-newline nil)))
  )
