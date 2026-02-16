;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message t         ; no splash screen
      maximise-on-startup nil           ; maximise emacs on startup
      )

;; first screen on startup ---
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "While any text editor can save your files, only Emacs can save your soul"
        dashboard-startup-banner 3
        dashboard-items '((recents   . 7)
                          (projects  . 5)
                          (agenda    . 5)))
  (dashboard-setup-startup-hook)
  (add-hook 'server-after-make-frame-hook (lambda () (dashboard-refresh-buffer)))
  )

;; scrolling ---
(use-package ultra-scroll
  :ensure t
  :config (ultra-scroll-mode 1))

;; fonts ---

;; for icons
(use-package nerd-icons
  :ensure t
  :config
  )
;; IMPORTANT: Run
;; (nerd-icons-install-fonts 1)
;; on installation, to install icons fonts.

;; Set font
(defvar my/font-height 130)

(when (eq system-type 'darwin)
  (setq my/font-height 140))

(when (member "FiraCode Nerd Font" (font-family-list))
  (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height my/font-height)
  (set-face-attribute 'fixed-pitch nil :family "FiraCode Nerd Font"))

(when (member "Open Sans" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Open Sans"))


(set-face-attribute 'default nil
					;; :family "JetBrainsMono Nerd Font"
                    :font "FiraCode Nerd Font"
					:height 130)  ;; 110 means 11pt (roughly)

;; allows you to mix fixed and variable pitched faces in Org and LaTeX mode.
(use-package mixed-pitch
  :ensure t
  :defer t
  :hook ((org-mode   . mixed-pitch-mode)
         (LaTeX-mode . mixed-pitch-mode)))

;; ligatures (specific config for Fira Code/Cascadia Code)
;; https://github.com/mickeynp/ligature.el/wiki
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

;; line numbers ---
;; display lines numbers (relative for vim bindings and absolute for emacs bindings)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-line-numbers-type (if (bound-and-true-p evil-mode)
                                                'relative
                                              'absolute)
                  display-line-numbers 1)))
;; default line number width to 3 to avoid jumping
(setq-default display-line-numbers-width 3)

;; themes ---

(use-package gruvbox-theme
  :ensure t)
(use-package catppuccin-theme
  :ensure t
  :config (setq catppuccin-flavor 'macchiato)) ;; from 'latte, 'frappe, 'macchiato, or 'mocha (brighter to darker)

;; (load-theme 'gruvbox :no-confirm)

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  :config
  (setq doom-theme 'doom-one)

  ;; Avoid loading theme in daemon TTY
  (unless (daemonp)
    (load-theme doom-theme t))

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Fix "unspecified-bg" error for GUI emacsclient frames
  (defun my/load-theme-for-server-frame ()
    (when (display-graphic-p)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme doom-theme t)))

  (add-hook 'server-after-make-frame-hook #'my/load-theme-for-server-frame)


  ;; ;; Ensure GUI frames get the correct theme after emacsclient connects
  ;; (defun my/load-theme-for-gui-frame (frame)
  ;;   (with-selected-frame frame
  ;;     (when (display-graphic-p frame)
  ;;       (mapc #'disable-theme custom-enabled-themes)
  ;;       (load-theme doom-theme t))))

  ;; ;; Use server-after-make-frame-hook specifically for emacsclient frames
  ;; (add-hook 'server-after-make-frame-hook #'my/load-theme-for-gui-frame)

  ;; ;; fix theme when frame created by emacsclient
  ;; (defun my/load-theme-on-gui ()
  ;;   (when (display-graphic-p)
  ;;     (mapc #'disable-theme custom-enabled-themes)
  ;;     (load-theme doom-theme t)))

  ;; (add-hook 'server-after-make-frame-hook #'my/load-theme-on-gui)
  )

;; add a border ---
(use-package spacious-padding
  :ensure t
  :config
  ;; Function to enable padding in a GUI frame
  (defun my/enable-spacious-padding-for-frame ()
    (when (display-graphic-p)
      (spacious-padding-mode 1)))

  ;; Enable for current frame if GUI
  (when (display-graphic-p)
    (spacious-padding-mode 1))

  ;; Also hook emacsclient frames
  (add-hook 'server-after-make-frame-hook #'my/enable-spacious-padding-for-frame)
  )
