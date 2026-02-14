(dolist (mode
         '(tool-bar-mode       ;; Remove toolbar
           scroll-bar-mode     ;; Remove scrollbars
           menu-bar-mode))     ;; Remove menu bar
  (funcall mode 0))
