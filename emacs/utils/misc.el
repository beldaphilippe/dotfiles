;; -*- lexical-binding: t; -*-

;; custom file ---
;; for the modifications made with emacs GUI
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; vertical line on programming buffers ---
(setq-default fill-column 100)
(setq-default display-fill-column-indicator-character ?│) ; use a thin vertical bar
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)  ; display a visual line at <fill-column> characters

;; line numbers ---
;; display lines numbers (relative for vim bindings and absolute for emacs bindings)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers-type (if (bound-and-true-p evil-mode)
                                                'relative
                                              'absolute)
                  display-line-numbers 1)))

;; extend pattern on visual line ---
(use-package extend-pattern
  :ensure nil
  :load-path "../ext"
  :hook (prog-mode . extend-pattern-mode))

;; tabulation width
(setq-default tab-width 4)
;; open URLs in the default web browser (not EWW) using XDG's utilace
(setq-default  browse-url-generic-program "xdg-open")
;; to edit gpg files, password asker
(setq-default epa-pinentry-mode 'loopback)
;; stop me from freezing emacs
(global-unset-key (kbd "C-z"))
;; yank at local cursor pos
(setq mouse-yank-at-point t)
;; top and bot line blink once on error/warning
(setq visible-bell t)

;; parenthesis ---
(show-paren-mode 1)       ;; matching parentheses
(setq show-paren-delay 0) ;; no delay
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-inside-paren t)
(electric-pair-mode 1);; auto close bracket insertion
(setq electric-pair-pairs
      (append electric-pair-pairs
              '((?\{ . ?\})
                (?' . ?'))))
(defun goto-matching-parenthesis (arg)
  "Go to the matching parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
;; Define subdirectories inside `user-emacs-directory`
(defvar my-backup-dir (expand-file-name "backups/" user-emacs-directory))
(defvar my-autosave-dir (expand-file-name "autosaves/" user-emacs-directory))
;; Create the directories if they don't exist
(unless (file-exists-p my-backup-dir) (make-directory my-backup-dir t))
(unless (file-exists-p my-autosave-dir) (make-directory my-autosave-dir t))
;; Configure Emacs to use them
(setq backup-directory-alist `((".*" . ,my-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))

;; aliases ---
(defalias 'ar #'align-regexp)

;; management of GC ---
(use-package gcmh
  :disabled t
  :ensure t
  :config
  (gcmh-mode 1))


(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode)
                         (highlight-indent-guides-mode))))
  :config (setq highlight-indent-guides-method                   'bitmap
                highlight-indent-guides-responsive               'top
                highlight-indent-guides-auto-enabled             nil
                highlight-indent-guides-bitmap-function          'highlight-indent-guides--bitmap-line
                highlight-indent-guides-auto-top-character-face-perc 100))

;; nice-looking modeline ---
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; nombre de match pour les recherches C-s affiché dans la modeline ---
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; syntax checking : errors and warnings
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; folds ---
(use-package hideshow
  :ensure t
  :config
  (setq hs-hide-comments-when-hiding-all nil)
  (setq hs-isearch-open t)
  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))
  :hook ((find-file-mode . hs-minor-mode)
	 (prog-mode . hs-minor-mode))
	 ;; (hs-minor-mode . hs-hide-all))
  :bind (:map hs-minor-mode-map
			  ("C-<tab>" . hs-toggle-hiding)
			  ("C-<iso-lefttab>" . hs-global-cycle)))

;; key bindings ---
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
