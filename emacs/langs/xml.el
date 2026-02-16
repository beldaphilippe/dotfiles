(use-package nxml-mode
  :defer t
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xsl\\'")
  :config
  ;; hideshow support
  (require 'hideshow)

  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"   ; start regexp
                 "-->\\|</[^/>]*[^/]>"   ; end regexp
                 "<!--"                  ; comment start
                 sgml-skip-tag-forward
                 nil))
  :hook
  (nxml-mode . hs-minor-mode)
  )
