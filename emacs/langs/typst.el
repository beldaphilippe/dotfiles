(use-package typst-ts-mode
  :ensure t
  :defer t
  :hook ((typst-ts-mode . eglot-ensure)
         (typst-ts-mode . hs-minor-mode))
  :custom (typst-ts-indent-offset 2)
  :config
  (ts-add-lang 'typst "https://github.com/uben0/tree-sitter-typst")
  (set-keymap-parent typst-ts-mode-map prog-mode-map)
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
  )
