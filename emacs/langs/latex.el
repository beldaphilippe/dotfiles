(add-hook 'latex-mode-hook 'flyspell-mode)

;; (use-package latex-ts-mode
;;   :ensure nil
;;   :load-path "../ext"
;; :init (unless (package-installed-p 'latex-ts-mode)
;;         (treesit-install-language-grammar
;;          '(nix "https://github.com/nix-community/tree-sitter-nix")))
;;   :mode ("\\.tex\\'" . latex-ts-mode)
  
;;   )
