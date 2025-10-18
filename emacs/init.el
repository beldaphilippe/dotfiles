;;; configuration --- Summary ;; -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; (setq byte-compile-warnings '(cl-functions))

(set-language-environment "UTF-8")

; for the modifications made with emacs GUI
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 (unless (file-exists-p custom-file)
   (write-region "" nil custom-file))
 (load custom-file)

;; CONFIG ===

;; Some config is also present in early-init.el
;; Note that this line uses setq-default rather than the setq that we have seen before
;; setq-default sets values only in buffers that do not have their own local values for the variable.

(setq-default ;; Interface
      recent-files-on-startup               t        ;; open a buffer with the recent files on startup
      maximise-on-startup                   nil      ;; maximize emacs when it starts
      truncate-partial-width-windows        nil      ;; ???
      display-graphic-p                     t        ;; for icons
      ring-bell-function                    t
      visible-bell                          t        ;; the screen blinks when you make an error
      vim-bindings                          nil      ;; enables vim bindings thanks to evil
      warning-minimum-level                 :error   ;; info level for the warning buffer to be popped
      completion-ignore-case                t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case    t
      mouse-yank-at-point                   t
      indent-tabs-mode                      nil      ;; indent with tabs
      ;; tab-width                             4        ;; default indent width
      c-basic-offset                        4        ;; tab width for c

                                                     ;; external utilities
      browse-url-generic-program                "xdg-open" ;; open URLs in the default web browser (not EWW) using XDG's utilace
      )

;; load all packages (lazy load for most)
(load (expand-file-name "packages.el" user-emacs-directory))

;; Define subdirectories inside `user-emacs-directory`
(defvar my-backup-dir (expand-file-name "backups/" user-emacs-directory))
(defvar my-autosave-dir (expand-file-name "autosaves/" user-emacs-directory))
;; Create the directories if they don't exist
(unless (file-exists-p my-backup-dir) (make-directory my-backup-dir t))
(unless (file-exists-p my-autosave-dir) (make-directory my-autosave-dir t))
;; Configure Emacs to use them
(setq backup-directory-alist `((".*" . ,my-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))

;; to edit gpg files, password asker
(setq epa-pinentry-mode 'loopbak)

;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;; from doom-start.el
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters can
;;   trigger/queue a superfluous (and expensive, depending on the window system)
;;   frame redraw at startup. The variables must be set to `nil' as well so
;;   users don't have to call the functions twice to re-enable them.
;; (push '(menu-bar-lines . 0)   default-frame-alist)
;; (push '(tool-bar-lines . 0)   default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)
;; (setq menu-bar-mode nil
;;       tool-bar-mode nil
;;       scroll-bar-mode nil)

(buffer-face-mode 1)      ;; so that different fonts can be used on different buffers if needed
(delete-selection-mode 1) ;; select text and automatically overwrite without typing backspace
(display-time-mode t)     ;; show time on powerbar
(show-paren-mode 1)       ;; matching parentheses
(electric-indent-mode -1)
(setq-default fill-column 100)
(display-fill-column-indicator-mode 1)  ; display a visual line at <fill-column> characters

(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers-type (if vim-bindings 'relative 'absolute)))) ;; display relative lines numbers
;(add-hook 'prog-mode-hook (lambda() (setq display-line-numbers-type 'relative))) ;; display relative lines numbers
(global-display-line-numbers-mode +1)                                            ;; display line numbers

(setq tramp-verbose 6)
(setq tramp-use-ssh-controlmaster-options nil)

;; INTERFACE ===

(if maximise-on-startup
        (push '(fullscreen . maximized) default-frame-alist))

(defalias 'ar #'align-regexp)

(defun goto-matching-parenthesis (arg)
  "Go to the matching parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(define-key input-decode-map [?\C-i] [C-i])                     ;; unbind C-i from TAB (GUI mode only)
(global-set-key (kbd "<C-i>") 'indent-relative)         ;; ... and remap it to indent-relative
(global-set-key (kbd "M-i") 'indent-region)                     ;; indent a region correctly
(global-set-key (kbd "C-ù") 'goto-matching-parenthesis)            ;; go to matching parenthesis
(global-set-key (kbd "C-c h") 'replace-string)          ;; replace string
(global-set-key [M-right] 'forward-sexp)
(global-set-key [M-left] 'backward-sexp)
(global-set-key (kbd "C-x t") 'treemacs)                        ;; toggle treemacs
(global-set-key (kbd "RET") 'newline-and-indent)        ;; indent after newline
(global-set-key (kbd "C-j") 'join-line)             ;; join line with previous one
(global-set-key (kbd "C-$") 'window-swap-states)    ;; join line with previous one


(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "<f5>")
              (lambda ()
                (interactive)
                (let* ((tex-file (buffer-file-name))
                       (default-directory (file-name-directory tex-file)))
                  (shell-command (format "pdflatex %s" (shell-quote-argument tex-file))))
                )))

;; remap of modifier keys
;;(setq x-ctrl-keysym 'meta)
;;(setq x-meta-keysym 'ctrl)
;;(setq x-capslock-keysym 'ctrl)


;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (keymap-set prog-mode-map "C-c p"
;;                         'backward-paragraph)
;;             (keymap-set prog-mode-map "C-c n"
;;                         'forward-paragraph)
;;             (keymap-set prog-mode-map "C-c C-x x" nil)))

;; THEMES ===


;; ;; Hook after theme loads
;; (add-hook 'after-load-theme-hook #'my/swap-centaur-tabs-colors)

;; (defun my/override-centaur-tabs-catppuccin ()
;;   "Override Catppuccin's centaur-tabs faces to match default background."
;;   (let ((bg (face-background 'default nil t))
;;         (fg (face-foreground 'default nil t)))
;;     ;; Make selected tab match buffer
;;     (set-face-attribute 'centaur-tabs-selected nil
;;                         :background bg
;;                         :foreground fg
;;                         :weight 'bold)

;;     ;; Unselected tabs can be slightly dimmer (optional)
;;     (set-face-attribute 'centaur-tabs-unselected nil
;;                         :background bg
;;                         :foreground (face-foreground 'shadow nil t))

;;     ;; Disable modified visual cues (optional)
;;     (set-face-attribute 'centaur-tabs-selected-modified nil
;;                         :inherit 'centaur-tabs-selected
;;                         :foreground fg)
;;     (set-face-attribute 'centaur-tabs-unselected-modified nil
;;                         :inherit 'centaur-tabs-unselected
;;                         :foreground (face-foreground 'shadow nil t))

;;     ;; Remove the modified marker triangle/dot
;;     (set-face-attribute 'centaur-tabs-modified-marker-selected nil
;;                         :inherit 'centaur-tabs-selected)
;;     (set-face-attribute 'centaur-tabs-modified-marker-unselected nil
;;                         :inherit 'centaur-tabs-unselected)))

;; ;; Apply after every theme load
;; (add-hook 'after-load-theme-hook #'my/override-centaur-tabs-catppuccin)


;; (use-package gruvbox-theme          :ensure t)
;; (use-package atom-one-dark-theme    :ensure t) ;; https://github.com/oneKelvinSmith/monokai-emacs
;; (use-package vscode-dark-plus-theme :ensure t) ;; https://github.com/ianyepan/vscode-dark-plus-emacs-theme
(use-package catppuccin-theme           :ensure t :config (setq catppuccin-flavor 'frappe)) ;; from 'latte, 'frappe, 'macchiato, or 'mocha (brighter to darker)
;; (use-package doom-themes                     :ensure t :config (setq doom-themes-enable-bold t doom-themes-enable-italic t )) ; if nil, italics is universally disabled
;; (use-package modus-vivendi-theme    :ensure t)
(load-theme (nth 4 '(gruvbox atom-one-dark vscode-dark-plus modus-vivendi catppuccin doom-one)) :no-confirm)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-palenight t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (nerd-icons must be installed!)
;;   ;; (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))


;; FONTS ===

;; Download a font from font from https://www.nerdfonts.com/font-downloads,
;; extract it and move a .ttf file to ~/.local/share/fonts/
;; Get the name of the font thanks to M-x describe-font.
;; To have icons, run M-x nerd-icons-install-fonts, then install the font generated.


(cond
 ((eq system-type 'windows-nt) ; Windows
  (set-frame-font "-outline-FiraCode Nerd Font-regular-normal-normal-mono-21-*-*-*-p-*-iso10646-1" t t))
 ((eq system-type 'darwin) ; macOS
  (set-frame-font "Menlo" t t))
 ((eq system-type 'gnu/linux) ; Linux
  (set-frame-font "-CTDB-FiraCode Nerd Font-regular-normal-normal-*-18-*-*-*-m-0-iso10646-1" t t)))

4;; (set-face-font 'default "-JB-JetBrainsMonoNL Nerd Font Mono-regular-normal-normal-*-18-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-DELV-OverpassM Nerd Font Mono-regular-normal-normal-*-18-*-*-*-m-0-iso10646-1")


;; (set-face-attribute 'default nil
;;      :family "Cascadia Code"
;;      :height 140
;;      :weight 'normal
;;      :width 'normal)

;; (set-face-attribute 'variable-pitch nil
;;                     :family "Lisnoti"
;;                     :height 1.0
;;                     :weight 'normal
;;                     :width 'normal)

;; (set-face-attribute 'fixed-pitch nil
;;                     :family "Mono"
;;                     :height 1.0
;;                     :weight 'normal
;;                     :width 'normal)


;; (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)
;; (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
;;; init.el ends here
