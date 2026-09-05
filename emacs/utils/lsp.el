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
(use-package eglot
  :hook ((python-mode   . eglot-ensure)
         (nix-ts-mode   . eglot-ensure)
         (java-ts-mode  . eglot-ensure)
         (c-ts-mode     . eglot-ensure)
         (c++-ts-mode   . eglot-ensure)
         (glsl-mode     . eglot-ensure)
         (typst-ts-mode . eglot-ensure)
         (latex-mode    . eglot-ensure)
         (js-mode       . eglot-ensure))
  ;; :custom
  ;; (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :config
  ;; disable inlay hints by default
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (eglot-inlay-hints-mode -1)))

  (dolist (lsp-entries '((typst-ts-mode . ("tinymist"))
                         (python-mode . ("pylsp"))
                         (nix-ts-mode . ("nixd"))
                         (glsl-mode . ("glsl_analyzer"))))
    (add-to-list 'eglot-server-programs lsp-entries))
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:autopep8    (:enabled :json-false)
                                                 :yapf       (:enabled :json-false)
                                                 :pylsp_black (:enabled t)))))
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
