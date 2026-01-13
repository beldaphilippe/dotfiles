(use-package typst-ts-mode
  :ensure t
  :config
  (require 'treesit)

  ;; Add nix grammar if not yet already assigned
  (unless (assoc 'typst treesit-language-source-alist)
    (add-to-list
     'treesit-language-source-alist
     '(typst "https://github.com/uben0/tree-sitter-typst")))

  ;; Only install Nix grammar if not already installed
  (unless (treesit-language-available-p 'typst)
    (treesit-install-language-grammar 'typst))

  (setq typst-preview-executable "~/added_modules/tinymist/tinymist")

  ;; ;; Eglot configuration for Typst
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list
  ;;    'eglot-server-programs
  ;;    `((typst-ts-mode)
  ;;      . ,(eglot-alternatives
  ;;          `(,typst-ts-lsp-download-path
  ;;            "tinymist"
  ;;            "typst-lsp")))))

  )

;; (use-package websocket)
;; (package-vc-install
;;  '(typst-preview :url "https://github.com/havarddj/typst-preview.el"))
;; (use-package typst-preview
;;   :after typst-ts-mode)
