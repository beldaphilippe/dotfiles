(use-package java-ts-mode
  :ensure t
  :config
  (ts-add-lang 'java "https://github.com/tree-sitter/tree-sitter-java")

  (setq auto-mode-alist (cons '("\\.deca$" . java-mode) auto-mode-alist))
  )
