(use-package js-ts-mode
  :defer t
  :hook
  (java-ts-mode . subword-mode)
  :config
  (ts-add-lang 'js "https://github.com/tree-sitter/tree-sitter-javascript")
  )
