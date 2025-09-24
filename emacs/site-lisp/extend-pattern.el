;;; extend-dashes --- Summary
;;; Commentary:
;;; This package implements a minor mode parsing through a specific pattern (default "===")
;;; and filling the rest of visual line with the last character of the pattern.
;;; The pattern is detected only if present in a commented line.
;;; It is implemented through overlays and thus does not affect the file.

;;; Code:

(defgroup extend-pattern nil
  "Extend pattern to fill visual line."
  :group 'alloc)

(defvar-local extend-pattern-mode nil
  "Toggle extend-symbol-mode.")
(defvar-local extend-pattern-which "==="
  "The pattern that is extended.  The last character is repeated to fill the visual line.")
(defvar-local extend-pattern-my-overlays '()
  "Contains all the overlays displaying the extended patterns.")

(defun extend-pattern--clear-all-my-overlays ()
  "Remove all overlays properly and allow Emacs to free them."
  (mapc #'delete-overlay extend-pattern-my-overlays) ;; Delete overlays from buffer
  (setq extend-pattern-my-overlays nil)) ;; Remove references (Emacs will GC them when needed)

(defun extend-pattern--create-overlays ()
  "Create overlays that extend a pattern visually to the window width."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((escaped-comment-start (regexp-quote (string-trim-right comment-start)))
           (escaped-comment-end (if comment-end (regexp-quote (string-trim-left comment-end)) ""))
           (pattern (regexp-quote extend-pattern-which))
           (regex (concat "^\\s-*"
                          escaped-comment-start
                          "\\(.*?\\)"              ; capture: text before pattern
                          pattern
                          "\\s-*"
                          escaped-comment-end
                          "\\s-*$")))
      (while (re-search-forward regex nil t)
        (let* ((start (match-beginning 0))
               (mid (match-end 1))                     ; just after captured label
               (line-end (line-end-position))
               (overlay (make-overlay mid line-end)))
          ;; replace everything after label with line fill
          (overlay-put overlay 'display
                       (make-string (max 0 (- (- (window-width) (current-column)) 3)) ?─))
          (push overlay extend-pattern-my-overlays))))))
	
(defun extend-pattern--update-overlays ()
  "Update all mode overlays."
  (interactive)
  (when extend-pattern-mode
    (extend-pattern--clear-all-my-overlays)
    (extend-pattern--create-overlays)))

;(defun toggle-extend-dashes-overlay ()
  ;"Toggle the \\'extend dashes\\' overlay dynamically."
  ;(interactive)
  ;(setq extend-dashes-overlay-active (not extend-dashes-overlay-active))
  ;(if extend-dashes-overlay-active
	  ;(progn
        ;(extend-dashes-overlay)
        ;(add-hook 'window-configuration-change-hook #'update-dashes-overlays))
    ;(progn
	  ;(clear-all-my-overlays)
	  ;(remove-hook 'window-configuration-change-hook #'update-dashes-overlays)))
  ;(message "Extend dashes overlay %s" (if extend-dashes-overlay-active "enabled" "disabled")))

(define-minor-mode extend-pattern-mode
  "Toggles local extend-pattern-mode."
  :lighter " extend pattern"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<dead-circumflex>") #'extend-pattern--update-overlays)
            map)

  (if extend-pattern-mode
      (add-hook 'window-configuration-change-hook #'extend-pattern--update-overlays) ; Force overlays to refresh dynamically after a window resize.
    (progn
      (remove-hook 'window-configuration-change-hook #'extend-pattern--update-overlays)
      (extend-pattern--clear-all-my-overlays))))
  
(provide 'extend-pattern)
;;; extend-pattern.el ends here
