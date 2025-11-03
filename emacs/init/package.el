;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "ext" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package) ;; loads use-package

;(setq-default
 ;use-package-always-ensure t
 ;use-package-compute-statistics t
 ;use-package-verbose t)
