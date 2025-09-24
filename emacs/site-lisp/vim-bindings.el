;;; vim-bindings --- Summary
;;; Commentary:
;;; Enables vim bindings through evil-mode.

;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
 ;; configuration of leader key
  (evil-set-leader '(normal visual) (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>qq") 'save-buffer-kill-terminal)
  (evil-define-key 'normal 'global (kbd "<leader>,") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>;") 'comment-line)
  (evil-define-key 'visual 'global (kbd "<leader>;") 'comment-dwim)
  (evil-define-key 'normal 'global (kbd "<leader>hk") 'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>hv") 'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hf") 'describe-function)
  (evil-define-key 'normal 'global (kbd "<leader>hm") 'describe-mode)
  (evil-define-key 'normal 'global (kbd "^") 'evil-first-non-blank-of-visual-line)
  (evil-define-key 'normal 'global (kbd "$") 'evil-end-of-visual-line)
  (evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line)
  (evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal 'global (kbd "<tab>") 'indent-for-tab-command)
  ;; configuration for org-mode
  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-enter-maybe-execute-code)
  (evil-set-undo-system 'undo-tree))

;; escape everything in Emacs with "kj"
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "kj"
                evil-escape-delay        0.2))

(use-package undo-tree
  :ensure t
  :after evil
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))


;;; vim-bindings.el ends here
