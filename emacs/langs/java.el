(use-package java-ts-mode
  :ensure t
  :hook
  (java-ts-mode . subword-mode)
  :config
  (ts-add-lang 'java "https://github.com/tree-sitter/tree-sitter-java")
  )
