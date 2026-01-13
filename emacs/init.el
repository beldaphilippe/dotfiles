;; -*- lexical-binding: t; -*-
;; loads all the configuration

(defun my/load (paths)
  "Load files in PATHS relatively to the user-emacs-directory."
  (when (not (listp paths))
    (setq paths (list paths)))
  (dolist (path paths)
    (condition-case err
        (load (expand-file-name path user-emacs-directory))
      (error (message "Failed to load %S: %S" path err)))))

;; files are loaded according to the list order
(my/load
  '("init/encoding.el"
    "init/package.el"
    "init/gui.el"))

(my/load
  '("utils/misc.el"
    "utils/completion.el"
    "utils/evil.el"
    "utils/live-server.el"))

(my/load
  '("langs/prog-mode.el"
    ;; "langs/c.el"
    "langs/glsl.el"
    "langs/java.el"
    ;; "langs/latex.el"
    ;; "langs/lisp.el"
    ;; "langs/markdown.el"
    "langs/nix.el"
    ;"langs/org.el"
    ;"langs/rmd.el"
    "langs/typst.el"
    "langs/xml.el"
    ))
