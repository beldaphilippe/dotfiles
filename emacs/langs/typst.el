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

  ;; :mode "\\.typst\\'"
  )
