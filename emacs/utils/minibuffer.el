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
  :init (vertico-mode)
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
  :after vertico
  :ensure t
  :init
  (vertico-posframe-mode 1)
  ;; :config
  ;; (defun my/vertico-posframe-get-size (buffer)
  ;;   "Function to be used by `vertico-posframe-size-function'."
  ;;   (list
  ;;    :height (buffer-local-value 'vertico-posframe-height buffer)
  ;;    :width (round (* (frame-width) 0.62))
  ;;    :min-height (or (buffer-local-value 'vertico-posframe-min-height buffer)
  ;;                    (let ((height (+ vertico-count 1)))
  ;;                      (min height (or (buffer-local-value 'vertico-posframe-height buffer) height))))
  ;;    :min-width (or (buffer-local-value 'vertico-posframe-min-width buffer)
  ;;                   (let ((width (round (* (frame-width) 0.62))))
  ;;                     (min width (or (buffer-local-value 'vertico-posframe-width buffer) width))))))

  ;; (setq  vertico-posframe-size-function #'my/vertico-posframe-get-size)
  )
(with-eval-after-load 'vertico (vertico-posframe-mode 1))

;; very useful
(use-package consult
  :ensure t
  ;; :bind (:map ctl-x-map
  ;;             ("b" . consult-buffer))
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
