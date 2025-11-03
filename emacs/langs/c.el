;; C configuration
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "<f5>") #'my/make-compile)
  (define-key c-mode-map (kbd "<f6>") #'my/make-run)
  (define-key c++-mode-map (kbd "<f5>") #'my/make-compile)
  (define-key c++-mode-map (kbd "<f6>") #'my/make-run))


(use-package irony
  :ensure t
  :hook
  (c++-mode-hook . irony-mode)
  (c-modeb-hook . irony-mode)
  (objc-mode-hook . irony-mode))
