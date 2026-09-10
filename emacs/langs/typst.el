(use-package typst-ts-mode
  :ensure t
  :defer t
  :hook (typst-ts-mode . hs-minor-mode)
  :custom
  (typst-ts-indent-offset 2)
  :config
  (ts-add-lang 'typst "https://github.com/uben0/tree-sitter-typst")
  )
