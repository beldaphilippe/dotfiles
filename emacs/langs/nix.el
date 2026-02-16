(use-package nix-ts-mode
  :ensure t
  :defer t
  :config
  (ts-add-lang 'nix "https://github.com/nix-community/tree-sitter-nix")

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

  :mode "\\.nix\\'")

