;;; early-init.el --- load before init.el ;; -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setenv "LSP_USE_PLISTS" "true")

(custom-set-variables
 ;; Performance settings
 ;; defer GC to shorten startup time
 '(gc-cons-threshold most-positive-fixnum)
 '(gc-cons-percentage 0.6)
 
 ;; Disable startup screen
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)
;; '(initial-scratch-message nil)

 ;; Frame settings
 '(initial-frame-alist '((left-fringe . 0)
			 (right-fringe . 0)
			 (internal-border-width . 8)
			 (tool-bar-lines . 0)))

 ;; Disable UI elements
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)

 ;; Buffer settings
 '(initial-buffer-choice t)
 '(initial-major-mode 'fundamental-mode)

 ;; Frame behavior
 '(frame-inhibit-implied-resize t)
 '(frame-title-format nil)
 '(cursor-in-non-selected-windows nil)

 ;; Font settings
 '(font-lock-maximum-size nil)
 '(x-underline-at-descent-line t)

 ;; Window divider settings
 '(window-divider-default-right-width 16)
 '(window-divider-default-places 'right-only))

;; For Emacs v29, remove the system bar on top of emacs
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; reset GC modification after the evaluation of early-init.el and init.el,
;; when emacs has finished starting
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(provide 'early-init)
;;; early-init.el ends here
