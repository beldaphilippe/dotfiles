;; -*- lexical-binding: t; -*-

;; colorful annotations on the minibuffer propositions
(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

;; minibuffer vertical interactive completion
(use-package vertico
  :ensure t
  :init (vertico-mode 1)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; when a directory's name is incomplete, delete the name completely in Vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)           ; make Return enter directories
        ("DEL" . vertico-directory-delete-char)     ; make Delete delete characters as well
        ("M-DEL" . vertico-directory-delete-word))  ; delete words easily
  :config
  (setq vertico-count 17)
  )

;; displays the minibuffer at window center
;; (use-package vertico-posframe
;;   :ensure t
;;   :after vertico
;;   )

(use-package consult
  :ensure t
  )

;; Completion style
;; Note: this package provides only a backend and must be used by a completion module
;;       such as Consult, Vertico or Company
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides '((file (styles basic
                                                 partial-completion
                                                 orderless))))
  (completion-pcm-leading-wildcard t)
  )
