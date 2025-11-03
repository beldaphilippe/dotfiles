;; -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

(defun my/compile-rmd ()
  "Compile the current R Markdown (.Rmd) file to pdf using Rscript and rmarkdown::render."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file (string-match "\\.Rmd\\'" file))
      (shell-command
       (format "Rscript -e \"rmarkdown::render('%s', 'pdf_document')\"" file)))))

;; (use-package ess
;;   :ensure t
;;   :init (require 'ess-site))

(define-derived-mode rmarkdown-ts-mode markdown-ts-mode "RMarkdown"
  "RMarkdown mode based on tree-sitter Markdown."
  (when (require 'ess-r-mode nil t)
    (setq-local markdown-code-lang-modes
                (append '(("r" . ess-r-mode))
                        markdown-code-lang-modes)))
  (setq-local markdown-fontify-code-blocks-natively t))

(add-to-list 'auto-mode-alist '("\\.[Rr]md\\'" . rmarkdown-ts-mode))


