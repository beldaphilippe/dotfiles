;; -*- lexical-binding: t; -*-
(setq-default
 completion-ignore-case                t
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case    t)

;; minibuffer ---
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)
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
(use-package vertico-posframe
  :ensure t
  :after vertico
  )

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

;; code completion ---
;; completion frontend suggestions
(use-package company
  :ensure t
  ;; :hook
  ;(prog-mode . company-mode)
  )

;; (use-package company
;;   :ensure t
;;   :after lsp-mode
;;   :hook
;;   (prog-mode . company-mode)
;;   (prog-mode . electric-pair-mode)
;;   ;; :bind (:map company-active-map
;;   ;;        ("<tab>" . company-complete-selection)
;;   ;;        :map lsp-mode-map
;;   ;;        ("<tab>" . company-indent-or-complete-common))
;;   :custom ;; like a setq in :config section
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0)
;;   (company-selection-wrap-around t)
;;   (company-frontends '(company-pseudo-tooltip-frontend
;;                        company-echo-metadata-frontend
;;                        company-tng-frontend))
;;   :config
;;   (company-tng-mode) ; use tab instead of C-n C-p CR
;;   ;; Optional: unbind C-n / C-p if you want to disable them in popup
;;   (define-key company-active-map (kbd "C-n") nil)
;;   (define-key company-active-map (kbd "C-p") nil))

;; better UI for company
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))


