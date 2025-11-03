
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
