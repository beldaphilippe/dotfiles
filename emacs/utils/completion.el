;; -*- lexical-binding: t; -*-

;; Code completion

(setq-default
 completion-ignore-case                t
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case    t
 )

(add-hook 'prog-mode-hook #'completion-preview-mode)

;; completion frontend suggestions
;; (use-package company
;;   :ensure t
;;   ;; :hook (prog-mode . company-mode)
;;   )

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
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))
