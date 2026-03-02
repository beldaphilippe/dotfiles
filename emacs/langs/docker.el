(use-package dockerfile-ts-mode
  :defer t
  :mode ("Dockerfile\\'")
  :config
  (ts-add-lang 'dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
  )
