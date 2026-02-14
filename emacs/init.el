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
 '(
   "init/encoding.el"                   ; first to set encoding (UTF-8)
   "init/package.el"                    ; load use-package
   "init/gui.el"                        ; graphical elements
   ))

(my/load
 '(
   "utils/modeline.el"
   "utils/minibuffer.el"
   "utils/misc.el"
   "utils/completion.el"
   "utils/evil.el"
   "utils/live-server.el"
   ))

(my/load
 (mapcar
  (lambda (file) (concat "langs/" file))
  '("utils.el"
    "c.el"
    "cpp.el"
    "git.el"
    "glsl.el"
    "java.el"
    ;; "latex.el"
    ;; "lisp.el"
    "markdown.el"
    "nix.el"
    "org.el"
    "python.el"
    ;; "rmd.el"
    "typst.el"
    "xml.el"
    )))
