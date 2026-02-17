;; -*- lexical-binding: t; -*-
;; Useful functions concerning languages

;; tree-sitter ---
(defun ts-add-lang (language grammar-path &optional branch subdir)
  "Add LANGUAGE grammar GRAMMAR-PATH to treesitter list, and install if not yet available."
  (interactive)
  (require 'treesit)

  ;; Add nix grammar if not yet already assigned
  (unless (assoc language treesit-language-source-alist)
    (add-to-list
     'treesit-language-source-alist
     `(,language ,grammar-path ,branch ,subdir)))

  ;; Only install Nix grammar if not already installed
  (unless (treesit-language-available-p language)
    (treesit-install-language-grammar language))
  )

;; Run file ---

(defun my/run (&optional buffer-pos interpreter options)
  "Run the current file in a buffer.

BUFFER-POS: optional, one of 'bg, 'front, or 'split.
INTERPRETER: optional, e.g. \"python\".
OPTIONS: optional command line options."
  (interactive
   (list
    'split
    nil
    nil))
    ;; (read-string "Interpreter (leave empty to run directly): ")
    ;; (read-string "Options: ")))

  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (unless (memq buffer-pos '(bg front split))
    (user-error "Invalid buffer-pos: %S (expected 'bg, 'front or 'split)" buffer-pos))

  ;; save buffer before run
  (save-buffer)

  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (filepath (buffer-file-name))
         (shell-buffer-name (format "*%s-shell*" filename))
         (command (string-join
                   (remove nil
                           (list
                            (unless (string-empty-p interpreter)
                              interpreter)
                            filepath
                            (unless (string-empty-p options)
                              options)))
                   " "))
         (shell-buffer (get-buffer-create shell-buffer-name))
         (already-running (comint-check-proc shell-buffer)))

    ;; Start or reuse shell
    (unless (comint-check-proc shell-buffer)
      (with-current-buffer shell-buffer
        (shell shell-buffer-name)))

    ;; Send command
    (with-current-buffer shell-buffer
      (goto-char (point-max))
      (comint-send-string shell-buffer (concat command "\n")))

    ;; Display behavior
    (if already-running
        (pcase buffer-pos
          ('bg nil)
          ('front (switch-to-buffer shell-buffer))
          ('split (display-buffer shell-buffer '(display-buffer-below-selected)))))

    (message "Command sent: %s" command)))

;; Makefile compile and run ---
(defun my/make-compile (&optional compile-option display nix-shell)
  "Call `make compile COMPILE-OPTION' if a makefile exists in the root directory of the project and optionally opens a shell buffer to display the results.

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
