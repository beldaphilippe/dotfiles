;; c++-ts-mode packaged with emacs
(ts-add-lang 'cpp "https://github.com/tree-sitter/tree-sitter-cpp")

;; c++-ts-mode packaged with emacs
(ts-add-lang 'cmake "https://github.com/uyha/tree-sitter-cmake")
(add-to-list 'auto-mode-alist '("CMake\\.txt\\'" . cmake-ts-mode))
