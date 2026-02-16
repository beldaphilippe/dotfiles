;; for GLSL, compile and run functions
(use-package glsl-mode
  :ensure t
  :defer t
  :bind (:map glsl-mode-map
              ("<f5>"   . (lambda () (interactive) (my/make-compile nil nil "shaders")))
              ("<S-f5>" . (lambda () (interactive) (my/make-compile nil t   "shaders")))
              ("<f6>"   . (lambda () (interactive) (my/make-run (format "FRAG_SHADER_PATH=%s" buffer-file-name) nil "shaders")))
              ("<S-f6>" . (lambda () (interactive) (my/make-run (format "FRAG_SHADER_PATH=%s" buffer-file-name) t   "shaders"))))
  :mode ("\\.\\(frag\\|vert\\)\\'" . glsl-mode))
