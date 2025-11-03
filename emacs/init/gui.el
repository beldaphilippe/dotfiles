;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message t         ; no splash screen
      maximise-on-startup nil           ; maximise emacs on startup
      )


;; first screen on startup
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "While any text editor can save your files, only Emacs can save your soul"
        dashboard-startup-banner 3
        dashboard-items '((recents   . 7)
                          (projects  . 5)
                          (agenda    . 5)))
  (dashboard-setup-startup-hook))


;; Disable bars
(defun simplify-ui (frame)
  "Remove from FRAME unwanted menubar/scrollbar/etc."
  (interactive)
  (modify-frame-parameters frame
                           '((menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'simplify-ui)
(when (display-graphic-p)
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'fringe-mode) (fringe-mode 1))
  (if (fboundp 'display-time-mode) (display-time-mode t)))

;; themes ---
(use-package gruvbox-theme
  :ensure t)
(use-package catppuccin-theme
  :ensure t
  :config (setq catppuccin-flavor 'macchiato)) ;; from 'latte, 'frappe, 'macchiato, or 'mocha (brighter to darker)
(load-theme 'gruvbox :no-confirm)

;; fonts ---

(set-face-attribute 'default nil
					:family "JetBrainsMono Nerd Font"
					:height 130)  ;; 110 means 11pt (roughly)
