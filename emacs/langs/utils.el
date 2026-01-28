;; Useful functions concerning languages

(defun ts-add-lang (language grammar-path)
  "Add LANGUAGE grammar GRAMMAR-PATH to treesitter list, and install if not yet available."
  (interactive)
  (require 'treesit)

  ;; Add nix grammar if not yet already assigned
  (unless (assoc language treesit-language-source-alist)
    (add-to-list
     'treesit-language-source-alist
     `(,language ,grammar-path)))

  ;; Only install Nix grammar if not already installed
  (unless (treesit-language-available-p language)
    (treesit-install-language-grammar language))
  )

(defun try-to-add-imenu ()
  "A menu to navigate between the functions of a file, when syntaxic coloration is available."
  (condition-case nil (imenu-add-to-menubar "Navigation") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;; Makefile compile and run ---
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

(defun my/make-run (&optional compile-option display nix-shell)
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
