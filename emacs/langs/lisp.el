(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq-local electric-pair-pairs
                (seq-remove (lambda (pair)
                              (eq (car pair) ?'))
                            electric-pair-pairs))
    (setq-local electric-pair-pairs
                (append electric-pair-pairs '((?\{ . ?\}))))))
