;;; latex-ts-mode.el --- Major mode for editing latex files powered by tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024-2025 Arthur Varon
;;
;; Author: Arthur Varon <arthur.varon@gmail.com>
;; Created: September 13, 2025
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A major-mode to integrate 'LaTeX-mode' based on 'treesit.el'.
;;
;;; Code:

;;(require 'latex)
;;(require 'font-latex)
(require 'treesit)


(defun latex-ts-setup ()
  "Setup treesit for latex-ts-mode"
  (treesit-parser-create 'latex)
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     latex-ts-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (section general)
                (environment def_ref generic file math)
                (param beamer)))

  ;; TODO: Indent rules!
 (setq-local treesit-simple-indent-rules latex-ts-indent-rules)

  (treesit-major-mode-setup))

(defcustom latex-ts-indent-level 2
  "Indentation per level in number of characters."
  :group 'latex-ts
  :safe 'wholenump
  :type 'integer)

(defvar latex-ts-indent-rules
  '((latex
     ((node-is "end") parent 0)
     ((parent-is "generic_environment") parent latex-ts-indent-level)
     ((parent-is "generic_command") parent latex-ts-indent-level)
     )))

(defvar latex-ts-font-lock-rules
  '(
    :language latex
    :feature general
    (
     (command_name) @font-lock-keyword-face

     (caption
      command: _ @font-lock-builtin-face)

     (graphics_include
      command: _ @font-lock-builtin-face
      )
     )
    :language latex
    :feature generic
    ( ;; strong enph underline /.... (TODO: should be using any-of? query with emacs 31.1 maybe?)
     ((generic_command
       command: (command_name) @font-lock-keyword-face
       arg: (curly_group
             (_) @font-latex-italic-face))
      (:match "\\emph" @font-lock-keyword-face))

     ((generic_command
       command: (command_name) @font-lock-keyword-face
       arg: (curly_group
             (_) @font-latex-italic-face))
      (:match "\\textit" @font-lock-keyword-face))

     ((generic_command
       command: (command_name) @font-lock-keyword-face
       arg: (curly_group
             (_) @font-latex-italic-face))
      (:match "\\mathit" @font-lock-keyword-face))

     ((generic_command
       command: (command_name) @font-lock-keyword-face
       arg: (curly_group
             (_) @font-latex-bold-face))
      (:match  "\\textbf" @font-lock-keyword-face))

     ((generic_command
       command: (command_name) @font-lock-keyword-face
       arg: (curly_group
             (_) @font-latex-bold-face))
      (:match  "\\mathbf" @font-lock-keyword-face))

     (generic_command
      (command_name) @font-lock-builtin-face
      (:match "^\\if[a-zA-Z@]+$" @font-lock-builtin-face))
     )
    ;; :language latex
    ;; :feature beamer
    ;; ( ; Beamer frames
    ;;  (generic_environment
    ;;   (begin
    ;;    name: (curly_group_text
    ;;           (text) @label)
    ;;    (:any-of? @label "frame"))
    ;;   .
    ;;   (curly_group
    ;;    (_) @markup.heading))
    ;;  ;; TODO
    ;;  ;; ((generic_command
    ;;  ;;   command: (command_name) @_name
    ;;  ;;   arg: (curly_group
    ;;  ;;         (_) @markup.heading))
    ;;  ;;  (#eq? @_name "\\frametitle"))
    ;;  )
    :language latex
    :feature file
    (; File inclusion commands
     (class_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (package_include
      command: _ @font-lock-builtin-face
      paths: (curly_group_path_list (_) @font-lock-string-face))

     (latex_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (verbatim_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (import_include
      command: _ @font-lock-builtin-face
      directory: (curly_group_path) @font-lock-string-face
      file: (curly_group_path (_) @font-lock-string-face))

     (bibstyle_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (bibtex_include
      command: _ @font-lock-builtin-face
      paths: (curly_group_path_list (_) @font-lock-string-face))

     (biblatex_include
      "\\addbibresource" @font-lock-builtin-face
      glob: (curly_group_glob_pattern (_) @font-lock-regexp-face))

     (graphics_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (svg_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (inkscape_include
      command: _ @font-lock-builtin-face
      path: (curly_group_path (_) @font-lock-string-face))

     (tikz_library_import
      command: _ @font-lock-builtin-face
      paths: (curly_group_path_list (_) @font-lock-string-face))
     )

    :language latex
    :feature comment
    ([(line_comment)
      (block_comment)
      (comment_environment)] @font-lock-comment-face)

    :language latex
    :feature section
    ((title_declaration
      command: _ @font-lock-keyword-face
      options: (brack_group
                (_) @font-latex-sectioning-1-face)
      :? text: (curly_group
                (_) @font-latex-sectioning-1-face))

     (author_declaration
      command: _ @font-lock-keyword-face
      authors: (curly_group_author_list
                (author)+ @font-latex-sectioning-1-face))

     (chapter
      command: _ @font-lock-keyword-face
      toc: (brack_group
            (_) @font-latex-sectioning-2-face)
      :? text: (curly_group
                (_) @font-latex-sectioning-2-face))

     (part
      command: _ @font-lock-keyword-face
      toc: (brack_group
            (_) @font-latex-sectioning-2-face)
      :? text: (curly_group
                (_) @font-latex-sectioning-2-face))

     (section
      command: _ @font-lock-keyword-face
      toc: (brack_group
            (_) @font-latex-sectioning-3-face)
      :? text: (curly_group
                (_) @font-latex-sectioning-3-face))

     (subsection
      command: _ @font-lock-keyword-face
      toc: (brack_group
            (_) @font-latex-sectioning-4-face)
      :? text: (curly_group
                (_) @font-latex-sectioning-4-face))

     (subsubsection
      command: _ @font-lock-keyword-face
      toc: (brack_group
            (_) @font-latex-sectioning-5-face)
      :? text: (curly_group
                (_) @font-latex-sectioning-5-face))

     )

    :language latex
    :feature environment
    (; General environments
     (begin
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (text) @font-lock-function-call-face ))

     (end
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (text) @font-lock-function-call-face ))
     )

    :language latex
    :feature def_ref
    ((label_definition
      command: _ @font-lock-keyword-face
      name: (curly_group_label
             (_) @font-lock-type-face))

     (label_reference_range
      command: _ @font-lock-keyword-face
      from: (curly_group_label
             (_) @font-lock-type-face)
      to: (curly_group_label
           (_) @font-lock-type-face))

     (label_reference
      command: _ @font-lock-keyword-face
      names: (curly_group_label_list
              (_) @font-lock-type-face))

     (label_number
      command: _ @font-lock-keyword-face
      name: (curly_group_label
             (_) @font-lock-type-face)
      number: (_) @font-lock-type-face)

     (citation
      command: _ @font-lock-keyword-face
      keys: (curly_group_text_list (_) @font-lock-type-face))

     ;; TODO
     ;; ((hyperlink
     ;;   command: _ @font-lock-keyword-face
     ;;   uri: (curly_group_uri
     ;;         (_) @font-lock-type-face)) @link
     ;;         (:set! @link url @font-lock-type-face))

     (glossary_entry_definition
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (_) @font-lock-type-face ))

     (glossary_entry_reference
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (_) @font-lock-type-face))

     (acronym_definition
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (_) @font-lock-type-face ))

     (acronym_reference
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (_) @font-lock-type-face))

     (color_definition
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (_) @font-lock-type-face))

     (color_reference
      command: _ @font-lock-keyword-face
      name: (curly_group_text
             (_) @font-lock-type-face)
      :?)
     )

    :language latex
    :feature math
    ([(displayed_equation)
      (inline_formula)] @font-latex-math-face

      (math_environment
       (_) @font-latex-math-face))

    :language latex
    :feature param
    (; Variables, parameters
     (placeholder) @font-lock-keyword-face

     (key_value_pair
      key: (_) @font-lock-keyword-face
      value: (_))

     (curly_group_spec
      (text) @font-lock-variable-use-face)

     (brack_group_argc) @font-lock-variable-use-face

     [
      (operator)
      "="
      "_"
      "^"
      ] @font-lock-operator-face

     "\\item" @font-lock-builtin-face

     (delimiter) @font-lock-delimiter-face

     (math_delimiter
      left_command: _ @font-lock-delimiter-face
      left_delimiter: _ @font-lock-delimiter-face
      right_command: _ @font-lock-delimiter-face
      right_delimiter: _ @font-lock-delimiter-face)

     [
      "["
      "]"
      "{"
      "}"
      ] @font-lock-punctuation-face
     )
    )
  )

;;;###autoload
(define-derived-mode latex-ts-mode LaTeX-mode "latex[ts]"
  "Major mode for editing LaTeX files powered by tree-sitter"
  :syntax-table latex-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'latex)
    (treesit-parser-create 'latex)
    (latex-ts-setup)))


(provide 'latex-ts-mode)

;;; latex-ts-mode.el ends here
