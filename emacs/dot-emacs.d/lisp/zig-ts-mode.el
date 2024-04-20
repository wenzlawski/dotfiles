;;; zig-ts-mode.el --- tree-sitter support for Zig   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marc Wenzlawski

;; Author: Marc Wenzlawski <marcwenzlawski@gmail.com>
;; Keywords: languages tree-sitter zig

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides tree-sitter support for Zig.

;;; Code:


(require 'treesit)
(require 'c-ts-common)
(eval-when-compile 'rx)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-parser-set-included-ranges "treesit.c")
(declare-function treesit-query-compile "treesit.c")

;;; Custom variables

(defcustom zig-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `zig-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'zig)

;;; Syntax table

(defvar rust-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 12"      table)
    (modify-syntax-entry ?*   "."      table)
    (modify-syntax-entry ?'   "\""     table)
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\\  "\\"     table)
    (modify-syntax-entry ?\n  ">"      table)
    table)
  "Syntax table for `rust-ts-mode'.")

;;; Indent

;; TODO: Adapt this to zig
(defvar zig-ts-mode--indent-rules
  `((zig
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "arguments") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "await_expression") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "array_expression") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "block") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "declaration_list") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "enum_variant_list") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "field_declaration_list") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "field_expression") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "field_initializer_list") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "let_declaration") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "macro_definition") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "parameters") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "struct_pattern") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "token_tree") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "use_list") parent-bol zig-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `zig-ts-mode'.")

;; TODO: Adapt this to zig
(defvar zig-ts-mode--keywords
  '("break" "return" "continue" "asm" "defer" "errdefer" "unreachable"
    "try" "catch" "async" "nosuspend" "await" "suspend" "resume"
    "const" "var" "extern" "packed" "export" "pub" "noalias" "inline"
    "noinline" "comptime" "callconv" "volatile" "allowzero"
    "align" "linksection" "threadlocal" "addrspace"
    "struct" "enum" "union" "error" "opaque"
    "if" "else" "switch" "and" "or" "orelse"
    "while" "for" "fn" "usingnamespace" "test"
    )
  "Zig keywords for tree-sitter font-locking.")

(defvar zig-ts-mode--operators '("=" "+" "*" "/" "%" "&" "|" "!" "<" ">")
  "Zig operators for tree-sitter font-locking.")

;; TODO: Adapt this to zig
(setq zig-ts-mode--font-lock-settings
      (treesit-font-lock-rules
       ;; :language 'zig
       ;; :feature 'attribute
       ;; '((attribute_item) @font-lock-preprocessor-face
       ;;   (inner_attribute_item) @font-lock-preprocessor-face)

       :language 'zig
       :feature 'bracket
       '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

       :language 'zig
       :feature 'builtin
       '(([(BUILTINIDENTIFIER)]) @font-lock-builtin-face)

       :language 'zig
       :feature 'comment
       '(([(line_comment)]) @font-lock-comment-face)

       ;; :language 'zig
       ;; :feature 'delimiter
       ;; '((["," "." ";" ":" "::"]) @font-lock-delimiter-face)

       :language 'zig
       :feature 'keyword
       `([,@zig-ts-mode--keywords] @font-lock-keyword-face)

       :language 'zig
       :feature 'number
       '([(FLOAT) (INTEGER)] @font-lock-number-face)

       :language 'zig
       :feature 'function
       '((SuffixExpr
	  variable_type_function: (IDENTIFIER) @font-lock-function-call-face)
	 [(FieldOrFnCall
           [
	    field_access: (IDENTIFIER) @font-lock-function-call-face
	    function_call: (IDENTIFIER) @font-lock-function-call-face])])
       
       :language 'zig
       :feature 'assignment
       '((VarDecl variable_type_function: (_) @font-lock-variable-name-face))

       :language 'zig
       :feature 'operator
       `([,@zig-ts-mode--operators] @font-lock-operator-face)

       :language 'zig
       :feature 'string
       '([(STRINGLITERALSINGLE)] @font-lock-string-face)

       :language 'zig
       :feature 'type
       '([(BuildinTypeExpr)] @font-lock-type-face)

       ;; :language 'zig
       ;; :feature 'variable
       ;; '((arguments (identifier) @font-lock-variable-use-face)
       ;;   (array_expression (identifier) @font-lock-variable-use-face)
       ;;   (assignment_expression right: (identifier) @font-lock-variable-use-face)
       ;;   (binary_expression left: (identifier) @font-lock-variable-use-face)
       ;;   (binary_expression right: (identifier) @font-lock-variable-use-face)
       ;;   (block (identifier) @font-lock-variable-use-face)
       ;;   (compound_assignment_expr right: (identifier) @font-lock-variable-use-face)
       ;;   (field_expression value: (identifier) @font-lock-variable-use-face)
       ;;   (field_initializer value: (identifier) @font-lock-variable-use-face)
       ;;   (if_expression condition: (identifier) @font-lock-variable-use-face)
       ;;   (let_condition value: (identifier) @font-lock-variable-use-face)
       ;;   (let_declaration value: (identifier) @font-lock-variable-use-face)
       ;;   (match_arm value: (identifier) @font-lock-variable-use-face)
       ;;   (match_expression value: (identifier) @font-lock-variable-use-face)
       ;;   (reference_expression value: (identifier) @font-lock-variable-use-face)
       ;;   (return_expression (identifier) @font-lock-variable-use-face)
       ;;   (tuple_expression (identifier) @font-lock-variable-use-face)
       ;;   (unary_expression (identifier) @font-lock-variable-use-face)
       ;;   (while_expression condition: (identifier) @font-lock-variable-use-face))
       ))


(defun zig-ts-mode--syntax-propertize (beg end)
  "Apply syntax properties to special characters between BEG and END.

Apply syntax properties to various special characters with
contextual meaning between BEG and END.

The apostrophe \\=' should be treated as string when used for char literals.

< and > are usually punctuation, e.g., as greater/less-than.  But
when used for types, they should be considered pairs.

This function checks for < and > in the changed RANGES and apply
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (search-forward "'" end t)
    (when (string-equal "char_literal"
                        (treesit-node-type
                         (treesit-node-at (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table (string-to-syntax "\""))))
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
            (treesit-node-parent
             (treesit-node-at (match-beginning 0))))
      ((or "type_arguments" "type_parameters")
       (put-text-property (match-beginning 0)
                          (match-end 0)
                          'syntax-table
                          (pcase (char-before)
                            (?< '(4 . ?>))
                            (?> '(5 . ?<))))))))


;;; Mode definition

;; TODO: 
;; (defvar-keymap zig-ts-mode-map
;;   :doc "Keymap for Zig language with tree-sitter."
;;   :parent prog-mode-map
;;   "C-c C-q" #'zig-mode-indent-defun
;;   "C-c ." #'zig-mode-set-style
;;   "C-c C-c" #'comment-region
;;   "C-c C-k" #'zig-mode-toggle-comment-style)


;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig"
  "Major mode for editing Zig, powered by tree-sitter."
  :group 'zig
  :syntax-table zig-ts-mode--syntax-table

  (when (treesit-ready-p 'zig)
    (treesit-parser-create 'zig)

    ;; Syntax.
    (setq-local syntax-propertize-function
                #'zig-ts-mode--syntax-propertize)

    ;; Comments.
    (c-ts-common-comment-setup)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings zig-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment )
                  ( keyword string )
                  ( assignment number builtin type )
                  ( bracket operator function )))

    ;; Imenu.
    ;; (setq-local treesit-simple-imenu-settings
    ;;             `(("Module" "\\`mod_item\\'" nil nil)
    ;;               ("Enum" "\\`enum_item\\'" nil nil)
    ;;               ("Impl" "\\`impl_item\\'" nil nil)
    ;;               ("Type" "\\`type_item\\'" nil nil)
    ;;               ("Struct" "\\`struct_item\\'" nil nil)
    ;;               ("Fn" "\\`function_item\\'" nil nil)))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules zig-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}():;,#" electric-indent-chars))

    ;; Navigation.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("enum_item"
			      "function_item"
			      "impl_item"
			      "struct_item")))
    (setq-local treesit-defun-name-function #'zig-ts-mode--defun-name)

    (treesit-major-mode-setup)))

;; TODO: Maybe do something like this?
;; (easy-menu-define c-ts-mode-menu (list c-ts-mode-map c++-ts-mode-map)
;;   "Menu for `c-ts-mode' and `c++-ts-mode'."
;;   '("C/C++"
;;     ["Comment Out Region" comment-region
;;      :enable mark-active
;;      :help "Comment out the region between the mark and point"]
;;     ["Uncomment Region" (comment-region (region-beginning)
;;                                         (region-end) '(4))
;;      :enable mark-active
;;      :help "Uncomment the region between the mark and point"]
;;     ["Indent Top-level Expression" c-ts-mode-indent-defun
;;      :help "Indent/reindent top-level function, class, etc."]
;;     ["Indent Line or Region" indent-for-tab-command
;;      :help "Indent current line or region, or insert a tab"]
;;     ["Forward Expression" forward-sexp
;;      :help "Move forward across one balanced expression"]
;;     ["Backward Expression" backward-sexp
;;      :help "Move back across one balanced expression"]
;;     "--"
;;     ("Style..."
;;      ["Set Indentation Style..." c-ts-mode-set-style
;;       :help "Set C/C++ indentation style for current buffer"]
;;      ["Show Current Indentation Style" (message "Indentation Style: %s"
;;                                                 c-ts-mode-indent-style)
;;       :help "Show the name of the C/C++ indentation style for current buffer"]
;;      ["Set Comment Style" c-ts-mode-toggle-comment-style
;;       :help "Toggle C/C++ comment style between block and line comments"])
;;     "--"
;;     ("Toggle..."
;;      ["SubWord Mode" subword-mode
;;       :style toggle :selected subword-mode
;;       :help "Toggle sub-word movement and editing mode"])))



(if (treesit-ready-p 'zig)
    (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode)))


(provide 'zig-ts-mode)
;;; zig-ts-mode.el ends here
