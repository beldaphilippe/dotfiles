(use-package rust-ts-mode
  :ensure t
  :defer t
  :config
  (ts-add-lang 'rust "https://github.com/tree-sitter/tree-sitter-rust")

  :mode "\\.rs\\'")
