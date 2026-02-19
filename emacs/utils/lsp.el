;; -*- lexical-binding: t; -*-

;; (require 'mardown-mode)
;; (require 'yasnippet)

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode)
;;   :config
;;   (setq
;;    ;; lsp-bridge-python-multi-lsp-server "basedpyright_ruff"
;;    lsp-bridge-enable-hover-diagnostic t
;;    )
;;   )

;; builtin
(use-package eglot)

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
