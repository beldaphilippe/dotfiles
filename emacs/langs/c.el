;; C configuration
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "<f5>") #'my/make-compile)
  (define-key c-mode-map (kbd "<f6>") #'my/make-run)
  (define-key c++-mode-map (kbd "<f5>") #'my/make-compile)
  (define-key c++-mode-map (kbd "<f6>") #'my/make-run))

;; company backend for C/C++
;; (use-package company-rtags
;;   :ensure t
;;   :after company
;;   :config (push 'company-rtags company-backends)
;;   )

(setq-default c-basic-offset 4)
(setq-default c-ts-mode-indent-offset 4)

(use-package irony
  :ensure t
  :defer t
  :hook
  (c++-mode-hook . irony-mode)
  (c-modeb-hook . irony-mode)
  (objc-mode-hook . irony-mode))


;; (use-package c-ts-mode
;;   :defer t
;;   :config
;;   (ts-add-lang 'c "https://github.com/tree-sitter/tree-sitter-c")
;;   )
