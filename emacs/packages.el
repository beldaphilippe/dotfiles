;;; packages --- Summary ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; a file to load all the packages needed, doing a bit of configuration
;;; NB: when using use-package, we can use ":defer t" to defer the loading
;;; of a package, and keywords ":mode :commands :hook :bind :interpreter :bind-keymap"

;;; TODO:

;;; Code:

;; package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))


;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; management of GC
(use-package gcmh
  :disabled t
  :ensure t
  :config
  (gcmh-mode 1))

;; Interface, fonts ===

;; relocate temporary and save files
;(use-package no-littering :ensure t)

;; first screen on startup
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents   . 7)
                          (projects  . 5)
                          (agenda    . 5))))

;; extend pattern on visual line
(use-package extend-pattern
  :ensure nil
  :load-path "site-lisp"
  :hook (prog-mode . extend-pattern-mode))

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


;; ligatures for fonts that support it
(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; nice-looking modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; tabs for emacs
(use-package centaur-tabs
  :disabled t
  :ensure t
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'over
        centaur-tabs-show-count nil
        ;; centaur-tabs-label-fixed-length 15
        ;; centaur-tabs-gray-out-icons 'buffer
        ;; centaur-tabs-plain-icons t
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  ;; make the active tab the same color as background
  (let ((bg1 (face-background 'default)))
    (set-face-attribute 'centaur-tabs-selected nil
                        :background bg1)
    (set-face-attribute 'centaur-tabs-selected-modified nil
                        :background bg1)
    (set-face-attribute 'centaur-tabs-close-selected nil
                        :background bg1)
    (set-face-attribute 'centaur-tabs-close-selected nil
                        :background bg1))
  (when vim-bindings
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd "g t") #'centaur-tabs-forward)
      (define-key evil-normal-state-map (kbd "g T") #'centaur-tabs-backward)))
        :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right))

;; nombre de match pour les recherches C-s affiché dans la modeline
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

;; nice icons for files types
;; (when (display-graphic-p) (use-package all-the-icons :ensure t))

;; magit for Git
(use-package magit
  :ensure t)
  ;; :custom
  ;; (custom-set-faces
  ;;    '(magit-diff-hunk-heading-highlight ((t (:background "#282c34"))))
  ;;    '(magit-diff-context-highlight ((t (:background "#3e4452"))))))

;; folder tree
(use-package treemacs
  :ensure t
  :bind ("C-x t" . treemacs)
  :init
  (treemacs-project-follow-mode)
  :config
  (defvar treemacs--project-follow-delay 0.2))
(use-package lsp-treemacs
  :after treemacs
  :ensure t)

;; colorful annotations on the minibuffer completion
(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

;; better minibuffer completion
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

;; enables vim bindings (if vim-bindings set to true)
(when vim-bindings
  (load (expand-file-name "site-lisp/vim-bindings.el" user-emacs-directory)))


;; Syntax, completion ===

;; vertical interactive completion
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
  (setq completion-styles '(flex))
  (setq completion-category-overrides '((file (styles . (partial-completion))))))

;; (use-package helm
;;   :ensure t
;;   :init
;;   (helm-mode 1)

;;   :config
;;   ;; Configure helm to hide buffers starting with *
;;   (setq helm-buffer-special-buffer-list '("^\\*"))

;;   ;; Optionally, you can customize the helm-buffer sources to exclude these buffers
;;   (setq helm-buffer-sources '(
;;                               helm-source-buffer-not-special
;;                               helm-source-buffers-list
;;                               )))


;; ;; Support de LSP pour tous les langages de programmation affichera
;; ;; une erreur si il ne trouve pas le client, mais cela devrait
;; ;; permettre d'exposer le plus de client possible.
;; ;; autodétecte et configure company
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook (prog-mode . lsp)
;;   ;; (kill-emacs . (lambda () (setq lsp-restart 'ignore))))
;;   :config
;;   (setq lsp-restart 'ignore) ;; request restart when lsp crashes
;;   (setq lsp-diagnostics-provider :flycheck)
;;   (setq lsp-diagnostics-flycheck-default-level 'error)
;;   (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . "lisp"))
;;   ;; per https://github.com/emacs-lsp/lsp-mode#performance

;;   (setq gc-cons-threshold 200000000) ;;200mb
;;   ;; (defun my/lsp-diagnostic-filter (diag)
;;   ;; "Only show LSP diagnostics with severity 1 (errors)."
;;   ;; (eq (gethash "severity" diag) 1))
;;   ;; (setq lsp-diagnostic-filter #'my/lsp-diagnostic-filter)
;;   )

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (prog-mode . (lambda ()
                 (unless (derived-mode-p 'emacs-lisp-mode) (lsp))))
  (glsl-mode . (lambda ()
                 (setq lsp-enable-links nil)))
  ;(before-save . lsp-format-buffer)
  :init
  (setq lsp-keymap-prefix "C-c C-l"
        lsp-diagnostics-provider :flycheck
        lsp-completion-provider :capf
        read-process-output-max (* 4 1024 1024) ;; 4mb
        lsp-log-io nil ;; Enable logging for debugging
        lsp-log-level 'debug
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-response-timeout 30
	backup-by-copying t) ; needed to avoid an error on file saving (cf https://github.com/emacs-lsp/lsp-mode/issues/3516)
  :custom
  (lsp-auto-configure t)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.1)
  (lsp-ui-doc-use-childframe t)
  (lsp-enable-folding nil)
  (lsp-enable-suggest-server-download t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics t)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-lens-enable nil))


;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred) ; defer load until commands are called
;;   :hook (prog-mode . (lambda ()
;;                        (unless (derived-mode-p 'emacs-lisp-mode) (lsp))))
;;   :init ;; evaluated before package loading
;;   (setq lsp-keymap-prefix "C-c C-l"
;;         lsp-diagnostics-provider :flycheck
;;         lsp-completion-provider :capf
;;         read-process-output-max (* 4 1024 1024) ;; 4mb
;;         lsp-log-io nil ;; for debugging, drastically affects performance
;;         lsp-typescript-update-imports-on-file-move-enabled 'never
;;         lsp-enable-file-watchers nil
;;         backup-by-copying t) ; needed to avoid an error on file saving (cf https://github.com/emacs-lsp/lsp-mode/issues/3516)
;;   :custom
;;   (lsp-auto-configure t)
;;   (lsp-keep-workspace-alive nil)                     ; Close LSP server if all project buffers are closed
;;   (lsp-idle-delay 0.1)                               ; Debounce timer for `after-change-function'
;;   (lsp-ui-doc-use-childframe t)
;;   (lsp-enable-folding nil)                           ; I disable folding since I use hideshow
;;   (lsp-enable-suggest-server-download t)             ; Useful prompt to download LSP providers
;;   (lsp-ui-sideline-show-hover nil)                   ; Sideline used only for diagnostics
;;   (lsp-ui-sideline-diagnostic-max-lines 20)          ; 20 lines since typescript errors can be quite big
;;   ;; completion
;;   (lsp-completion-enable t)
;;   (lsp-completion-enable-additional-text-edit t)     ; Ex: auto-insert an import for a completion candidate
;;   (lsp-enable-snippet t)                             ; Important to provide full JSX completion
;;   ;; headerline
;;   (lsp-headerline-breadcrumb-enable nil)             ; Optional, I like the breadcrumbs
;;   (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
;;   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (lsp-headerline-breadcrumb-icons-enable nil)
;;   ;; lens
;;   (lsp-lens-enable nil)                              ; Optional, I don't need it
;;   :preface
;;   (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;     "Try to parse bytecode instead of json."
;;     (or
;;      (when (equal (following-char) ?#)

;;        (let ((bytecode (read (current-buffer))))
;;          (when (byte-code-function-p bytecode)
;;            (funcall bytecode))))
;;      (apply old-fn args)))
;;   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;     "Prepend emacs-lsp-booster command to lsp CMD."
;;     (let ((orig-result (funcall old-fn cmd test?)))
;;       (if (and (not test?)                             ;; for check lsp-server-present?
;;                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;                lsp-use-plists
;;                (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;                (executable-find "emacs-lsp-booster"))
;;           (progn
;;             (message "Using emacs-lsp-booster for %s!" orig-result)
;;             (cons "emacs-lsp-booster" orig-result))
;;         orig-result)))
;;   :init
;;   (setq lsp-use-plists t)
;;   ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
;;   (advice-add (if (progn (require 'json)
;;                          (fboundp 'json-parse-buffer))
;;                   'json-parse-buffer
;;                 'json-read)
;;               :around
;;               #'lsp-booster--advice-json-parse)
;;   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
  

;; syntax checking : errors and warnings
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; code completion : suggestions
(use-package company
  :ensure t
  :after lsp-mode
  :hook
  (prog-mode . company-mode)
  (prog-mode . electric-pair-mode)
  ;; :bind (:map company-active-map
  ;;        ("<tab>" . company-complete-selection)
  ;;        :map lsp-mode-map
  ;;        ("<tab>" . company-indent-or-complete-common))
  :custom ;; like a setq in :config section
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-selection-wrap-around t)
  (company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend
                       company-tng-frontend))
  :config
  (company-tng-mode) ; use tab instead of C-n C-p CR
  ;; Optional: unbind C-n / C-p if you want to disable them in popup
  (define-key company-active-map (kbd "C-n") nil)
  (define-key company-active-map (kbd "C-p") nil))

;; better UI for company
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))
  
  
;; converts a document to html and allow edition
(use-package htmlize
  :ensure t
  :commands (htmlize-buffer))

;; html live server ===

(defvar my/live-server-process nil
  "Handle for the live-server process.")

(defun my/start-live-server ()
  "Start live-server in the project root and open browser."
  (interactive)
  (let ((default-directory (or (lsp-workspace-root) default-directory)))
    (when (process-live-p my/live-server-process)
      (kill-process my/live-server-process))
    (setq my/live-server-process
          (start-process-shell-command
           "live-server" "*live-server*"
           "browser-sync start --server --files '**/*.html' '**/*.css' '**/*.js'"))
    (message "Started live-server on http://localhost:3000")))

(defun my/stop-live-server ()
  "Stop the running live-server process."
  (interactive)
  (when (process-live-p my/live-server-process)
    (kill-process my/live-server-process)
    (message "Stopped live-server.")))

(with-eval-after-load 'sgml-mode
  (define-key html-mode-map (kbd "<f5>") #'my/start-live-server))


;; SPECIFIC FILES MODES ===

;; Un menu pour naviguer entre les fonctions dans un fichier (Python,
;; Ada, C, ...). On l'ajoute pour tous les modes ayant de la
;; coloration syntaxique :
(defun try-to-add-imenu ()
  "A menu to navigate between the functions of a file, when syntaxic coloration is available."
  (condition-case nil (imenu-add-to-menubar "Navigation") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)


(defun my/run-nix-develop (shell-buffer nix-shell-name)
  "Run 'nix develop NIX-SHELL-NAME' in the given SHELL-BUFFER.
NIX-SHELL-NAME should be a valid flake reference like 'shaders'."
  (when (and (buffer-live-p shell-buffer)
             (comint-check-proc shell-buffer))
    (with-current-buffer shell-buffer
      (let ((proc (get-buffer-process shell-buffer)))
        (when proc
          (comint-send-string proc (format "nix develop ~/my-nixos-config#%s\n" nix-shell-name))
          (goto-char (point-max)))))))


;; for C and C++, compile and run functions
(defun my/make-compile (&optional compile-option display nix-shell)
  "Call `make compile COMPILE-OPTION' if a makefile exists
in the root directory of the project and optionally opens a shell buffer to display the results.

If DISPLAY is non-nil, opens the shell buffer in the right half of the current buffer."
  (interactive "sCompile option: \nP")
  (let* ((root (project-root (project-current)))
         (buf-name (format "*%s-shell*" (file-name-nondirectory (directory-file-name (file-name-directory root)))))
         (shell-buffer (get-buffer-create buf-name))
         (current-win (selected-window)))
    (if (not (file-exists-p (expand-file-name "Makefile" root)))
        (message (format "No Makefile found at project root: %s" root))
      ;; Starts shell in project root directory if not running
      (unless (comint-check-proc shell-buffer)
        (with-current-buffer shell-buffer
          (let ((default-directory root))
            (shell shell-buffer))
          ;; Run nix develop
          (when nix-shell
            (my/run-nix-develop shell-buffer nix-shell))))

      ;; Run command
      (with-current-buffer shell-buffer
        (let ((proc (get-buffer-process shell-buffer)))
          (when proc
            (comint-send-string proc (format "make compile %s\n" (or compile-option "")))
            (goto-char (point-max)))))

      ;; Display buffer if display is non-nil
      (if display
        (let ((win (display-buffer
                    shell-buffer
                    '((display-buffer-reuse-window
                       display-buffer-at-bottom)
                      (window-height . 0.3)))))
          (select-window current-win))
        (message "Buffer file compiled.")
        ))))

(defun my/make-run (&optional compile-option display)
  "Call `make run COMPILE-OPTION' if a makefile exists in the root directory of the project and optionally opens a shell buffer to display the results.

If DISPLAY is non-nil, opens the shell buffer in the right half of the current buffer."
  (interactive "sCompile option: \nP")
  (let* ((root (project-root (project-current)))
         (buf-name (format "*%s-shell*" (file-name-nondirectory (directory-file-name (file-name-directory root)))))
         (shell-buffer (get-buffer-create buf-name))
         (current-win (selected-window)))
    (if (not (file-exists-p (expand-file-name "Makefile" root)))
        (message (format "No Makefile found at project root: %s" root))
      ;; Starts shell in project root directory if not running
      (unless (comint-check-proc shell-buffer)
        (with-current-buffer shell-buffer
          (let ((default-directory root))
            (shell shell-buffer))
          ;; Run nix develop
          (when nix-shell
            (my/run-nix-develop shell-buffer nix-shell))))

      ;; Run command
      (with-current-buffer shell-buffer
        (let ((proc (get-buffer-process shell-buffer)))
          (when proc
            (comint-send-string proc (format "make run %s\n" (or compile-option "")))
            (goto-char (point-max)))))

      ;; Display buffer if display is non-nil
      (if display
        (let ((win (display-buffer
                    shell-buffer
                    '((display-buffer-reuse-window
                       display-buffer-at-bottom)
                      (window-height . 0.3)))))
          (select-window current-win))
        (message "Buffer file ran.")
        ))))

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "<f5>") #'my/make-compile)
  (define-key c-mode-map (kbd "<f6>") #'my/make-run)
  (define-key c++-mode-map (kbd "<f5>") #'my/make-compile)
  (define-key c++-mode-map (kbd "<f6>") #'my/make-run))

;; for GSLS, compile and run functions
(use-package glsl-mode
  :ensure t
  :bind (:map glsl-mode-map
              ("<f5>"   . (lambda () (interactive) (my/make-compile nil nil "shaders")))
              ("<S-f5>" . (lambda () (interactive) (my/make-compile nil t   "shaders")))
              ("<f6>"   . (lambda () (interactive) (my/make-run (format "FRAG_SHADER_PATH=%s" buffer-file-name) nil "shaders")))
              ("<S-f6>" . (lambda () (interactive) (my/make-run (format "FRAG_SHADER_PATH=%s" buffer-file-name) t   "shaders"))))
  :mode ("\\.\\(frag\\|vert\\)\\'" . glsl-mode))

;; (add-to-list 'auto-mode-alist '("\\.[Rr]md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

;; ORG mode live execution
(use-package org-modern
  :ensure t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (C . t)
     (python . t)
     (emacs-lisp . t)))
  :hook
  (org-mode . org-indent-mode)
  (org-mode . org-modern-mode)
  ;;(org-mode . (lambda () (local-set-key (kbd "TAB") 'org-fold-or-unfold-heading)))
  (org-mode . visual-line-mode)
  :config
  (declare-function org-export-to-file "org-mode")
  (setq browse-url-browser-function #'browse-url-firefox)
  (setq org-modern-star '("✪" "✪" "✪" "✪" "✪"))
  (setq org-image-actual-width 500)
  (setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0
      org-confirm-babel-evaluate nil
   org-startup-indented t
      org-hide-emphasis-markers t)
  :custom
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.30))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.20))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.15))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.10))))
   '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
   '(org-level-7 ((t (:inherit outline-7 :height 1.00))))))

;; Python configuration
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; C configuration
(use-package irony
  :ensure t
  :hook
  (c++-mode-hook . irony-mode)
  (c-modeb-hook . irony-mode)
  (objc-mode-hook . irony-mode))


;; ;; package for markdown mode
;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init (setq markdown-command "multimarkdown")
;;   :bind (:map markdown-mode-map
;;          ("C-c C-e" . markdown-do)))
;; (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;; package for markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Rmd compilation
(defun my/compile-rmd ()
  "Compile the current R Markdown (.Rmd) file to HTML using Rscript and rmarkdown::render."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file (string-match "\\.Rmd\\'" file))
      (shell-command
       (format "Rscript -e \"rmarkdown::render('%s')\"" file)))))

;; R modes
;; (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-noweb+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown+r-mode))

;; Latex
(add-hook 'latex-mode-hook 'flyspell-mode)


;; Overlays ===

;; folds
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

;;(provide 'packages)
;;; packages.el ends here
