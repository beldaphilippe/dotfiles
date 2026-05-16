(use-package typst-ts-mode
  :ensure t
  :defer t
  :hook (typst-ts-mode . hs-minor-mode)
  :config
  (ts-add-lang 'typst "https://github.com/uben0/tree-sitter-typst")
  )
