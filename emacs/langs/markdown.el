;; ;; package for markdown mode
(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . olivetti-mode)
)

;; package for markdown mode
(use-package markdown-ts-mode
  :ensure t
  :defer t
  :disabled t
  :hook (markdown-mode . olivetti-mode)
  :config
  (ts-add-lang 'markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
  (ts-add-lang 'markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
  :mode ("\\.md\\'" . markdown-ts-mode)
  )
