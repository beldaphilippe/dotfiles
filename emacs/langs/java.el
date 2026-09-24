(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(use-package java-ts-mode
  :ensure t
  :defer t
  :hook ((java-ts-mode . subword-mode)
         (java-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(java-ts-mode . ("jdtls")))
  (ts-add-lang 'java "https://github.com/tree-sitter/tree-sitter-java" nil nil nil)
  )
