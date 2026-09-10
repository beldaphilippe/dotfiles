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
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; when a directory name is incomplete, delete the name completely in Vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)           ; make Return enter directories
        ("DEL" . vertico-directory-delete-char)     ; make Delete delete characters as well
        ("M-DEL" . vertico-directory-delete-word))  ; delete words easily
  :config
  (setq vertico-count 20)
  )

(use-package vertico-buffer
  :after vertico
  :config
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (side . bottom)
          (window-height . 0.3)
          (window-parameters . ((mode-line-format . none)
                                (header-line-format . none)))))
  (vertico-buffer-mode 1)
  )


;; change minibuffers colors to stand out

(defface my/echo-area-bg
  '((t :extend t))
  "Background face for the echo area / minibuffer.")
(defface my/minibuffer-highlight
  '((t))
  "Highlight face override, scoped to the minibuffer.")
(defvar my/minibuffer-face-cookies nil)

(defun my/echo-area-set-bg ()
  (set-face-attribute 'my/echo-area-bg nil
                       :background (face-attribute 'highlight :background nil t))
  (set-face-attribute 'my/minibuffer-highlight nil
                       :background (face-attribute 'mode-line :background nil t))
  (setq my/minibuffer-face-cookies
        (list (face-remap-add-relative 'default 'my/echo-area-bg)
              (face-remap-add-relative 'highlight 'my/minibuffer-highlight))))

(defun my/echo-area-clear-bg ()
  (mapc #'face-remap-remove-relative my/minibuffer-face-cookies)
  (setq my/minibuffer-face-cookies nil))

(add-hook 'minibuffer-setup-hook #'my/echo-area-set-bg)
(add-hook 'minibuffer-exit-hook #'my/echo-area-clear-bg)

;; displays the minibuffer at window center
(use-package vertico-posframe
  :disabled t
  :ensure t
  :init
  (setq vertico-posframe-parameters   `((left-fringe  . 12)    ;; Fringes
                                        (right-fringe . 12)
                                        ))
  ;; (cursor-color . ,(face-attribute 'cursor :background nil t))))
  :config
  (vertico-posframe-mode 1)
  (setq ;vertico-posframe-width        96                       ;; Narrow frame
   vertico-posframe-height       vertico-count            ;; Default height
   ;; Don't create posframe for these commands
   vertico-multiform-commands    '((consult-line    (:not posframe))
                                   (consult-ripgrep (:not posframe)))))

;; very useful
(use-package consult
  :ensure t
  :defer t
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
