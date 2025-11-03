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
