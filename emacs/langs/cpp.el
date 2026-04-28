;; c++-ts-mode packaged with emacs
(use-package c++-ts-mode
  :defer t
  :mode ("\\.cpp\\'" "\\.c++\\'" "\\.cxx\\'" "\\.hpp\\'" "\\.hxx\\'" "\\.tpp\\'" "\\.txx\\'""\\.ipp\\'" "\\.ixx\\'")
  :config
  ;; dont know why but c grammar needed
  (ts-add-lang 'c "https://github.com/tree-sitter/tree-sitter-c")
  (ts-add-lang 'cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  )

(use-package cmake-ts-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'")
  :config
  (ts-add-lang 'cmake "https://github.com/uyha/tree-sitter-cmake")
  )
