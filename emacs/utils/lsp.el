;; -*- lexical-binding: t; -*-

;; builtin
(use-package eglot
  ;; :custom
  ;; (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :config
  ;; format buffer on save
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  ;; disable inlay hints by default
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (eglot-inlay-hints-mode -1)))
  )

;; syntax checking : errors and warnings
(use-package flycheck
  :ensure t
  ;; :init (global-flycheck-mode)
  )

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1)
  )

;; support for direnv
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode)
  )
