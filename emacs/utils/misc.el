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

;; extend pattern on visual line ---
(use-package extend-pattern
  :ensure nil
  :load-path "../ext"
  :hook (prog-mode . extend-pattern-mode))

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
(setq-default show-paren-delay 0 ;; no delay
              show-paren-style 'parenthesis
              show-paren-when-point-inside-paren t)
(electric-pair-mode 1);; auto close bracket insertion
(setq electric-pair-pairs
      (append electric-pair-pairs
              '((?\{ . ?\}))))
                ;; (?' . ?'))))

(defun goto-matching-parenthesis (arg)
  "Go to the matching parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; backups files ---
;; Define subdirectories inside `user-emacs-directory`
(defvar my-backup-dir (expand-file-name "backups/" user-emacs-directory))
(defvar my-autosave-dir (expand-file-name "autosaves/" user-emacs-directory))
;; Create the directories if they don't exist
(unless (file-exists-p my-backup-dir) (make-directory my-backup-dir t))
(unless (file-exists-p my-autosave-dir) (make-directory my-autosave-dir t))
;; Configure Emacs to use them
(setq backup-directory-alist `((".*" . ,my-backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))

;; align regexp ---
(setq-default align-default-spacing 1)

(defun align-regexp-repeat (beg end regexp
                               &optional group spacing)
  "Wrapper for align-regexp with repeat."
  (interactive
   (list (region-beginning)
         (region-end)
         (concat "\\(\\s-*\\)"
                 (read-string "Align regexp: "))
         1
         align-default-spacing))
  (align-regexp beg end regexp group spacing t))

(defun align-table (beg end)
  "Align fields after commas.
Keeps commas attached to the preceding field."
  (interactive "r")
  (align-regexp beg end
                ",\\(\\s-*\\)"
                1 1 t))

(defalias 'ar #'align-regexp-repeat)

;; management of GC ---
(use-package gcmh
  :disabled t
  :ensure t
  :config
  (gcmh-mode 1))

;; indentation guides ---
(use-package highlight-indent-guides
  :ensure t
  :defer t
  ;; :hook
  ;; (prog-mode . (lambda ()
  ;;                (unless (derived-mode-p 'emacs-lisp-mode
  ;;                                        'nix-ts-mode)
  ;;                  (highlight-indent-guides-mode 1))))
  :custom
  (highlight-indent-guides-delay 0)     ; time to wait before refreshing
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive 'top)
  
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-character-face-perc 30)
  (highlight-indent-guides-auto-top-character-face-perc 100)
  )

;; nombre de match pour les recherches C-s affiché dans la modeline ---
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))


;; (defun rename-current-buffer-file ()
(defun mv ()
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

;; syntax checking : errors and warnings ---
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; trailing whitespaces ---
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode)
  :custom
  ;; Only trim whitespace outside of string literals
  (ws-butler-trim-predicate
   (lambda (beg end)
     "Trim whitespace only if region is not part of a string."
     (not (eq 'font-lock-string-face
              (get-text-property end 'face)))))

  ;; Optional: preserve whitespace before point
  ;; (ws-butler-keep-whitespace-before-point t)
  )

;; adding new line at the end of file ---
(setq-default require-final-newline t)

;; tabs ---
(setq-default tab-width 4
              indent-tabs-mode nil)

;; centers text ---
(use-package olivetti
  :defer t
  :config
  (setq olivetti-style t))

;; folds ---
(use-package hideshow
  :ensure t
  :hook (prog-mode . hs-minor-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil)
  (setq hs-isearch-open t)

  ;; Cycle inside the current nesting level
  (defun hs-cycle-current-level ()
    "Cycle hideshow states at the current nesting level."
    (interactive)
    (save-excursion
      ;; Move to beginning of current block or stay where we are
      (let* ((initial-point (point))
             (block-start (progn
                            (ignore-errors (hs-find-block-beginning))
                            (point)))
             (command (if (eq last-command 'hs-cycle-current-level)
                          this-command
                        'start)))

        (pcase command
          ;; First call → toggle this block normally
          ('start
           (hs-toggle-hiding)
           (setq this-command 'hs-cycle-current-level))

          ;; Second call → hide children inside this block
          ('hs-cycle-current-level
           (goto-char block-start)
           (hs-hide-level 1))

          ;; Third call → show this block
          ('hs-cycle-current-level
           (goto-char block-start)
           (hs-show-block))

          ;; Fourth call → show everything in this nesting level
          ('hs-cycle-current-level
           (goto-char block-start)
           (hs-show-level 1))))))

  ;; Replace your old hs-global-cycle binding here
  ;; :hook ((find-file . hs-minor-mode)
  ;;        (prog-mode . hs-minor-mode))
  :bind (:map hs-minor-mode-map
              ("C-<tab>" . hs-toggle-hiding)
              ("C-<iso-lefttab>" . hs-cycle-current-level)))

;; (use-package hideshow
;;   :ensure t
;;   :config
;;   (setq hs-hide-comments-when-hiding-all nil)
;;   (setq hs-isearch-open t)
;;   (defun hs-global-cycle ()
;;     (interactive)
;;     (pcase last-command
;;       ('hs-global-cycle
;;        (save-excursion (hs-show-all))
;;        (setq this-command 'hs-global-show))
;;       (_ (hs-hide-all))))
;;   :hook ((find-file-mode . hs-minor-mode)
;; 	 (prog-mode . hs-minor-mode))
;; 	 ;; (hs-minor-mode . hs-hide-all))
;;   :bind (:map hs-minor-mode-map
;; 			  ("C-<tab>" . hs-toggle-hiding)
;; 			  ("C-<iso-lefttab>" . hs-global-cycle)))

;; key bindings ---

(global-set-key (kbd "M-i") 'indent-relative)           ;;  indent-relative
(global-set-key (kbd "C-x b") 'consult-buffer)          ;; replace string
(global-set-key (kbd "C-x C-b") 'consult-buffer)        ;; avoid fat fingers
(global-set-key (kbd "C-ù") 'goto-matching-parenthesis) ;; go to matching parenthesis
(global-set-key (kbd "C-c h") 'replace-string)          ;; replace string
(global-set-key [M-right] 'forward-sexp)
(global-set-key [M-left] 'backward-sexp)
(global-set-key (kbd "C-x t") 'treemacs)         ;; toggle treemacs
(global-set-key (kbd "RET") 'newline-and-indent) ;; indent after newline
(global-set-key (kbd "C-j") 'join-line)          ;; join line with previous one
(global-set-key (kbd "C-$") 'window-swap-states) ;; join line with previous one
