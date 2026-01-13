(use-package java-ts-mode
  :ensure t
  :config
  (require 'treesit)

  ;; Add nix grammar if not yet already assigned
  (unless (assoc 'java treesit-language-source-alist)
    (add-to-list
     'treesit-language-source-alist
     '(java "https://github.com/tree-sitter/tree-sitter-java")))

  ;; Only install Nix grammar if not already installed
  (unless (treesit-language-available-p 'java)
    (treesit-install-language-grammar 'java))

  (setq auto-mode-alist (cons '("\\.deca$" . java-mode) auto-mode-alist))
  )
