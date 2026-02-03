;; package for markdown mode
(use-package markdown-ts-mode
  :ensure t
  :defer 't
  :config
  (ts-add-lang 'markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
  (ts-add-lang 'markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
  :mode ("\\.md\\'" . markdown-ts-mode)
  )
