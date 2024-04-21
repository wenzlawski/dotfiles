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

(defcustom zig-zig-bin "zig"
  "Path to zig executable."
  :type 'file
  :safe #'stringp)

(defcustom zig-run-optimization-mode "Debug"
  "Optimization mode to run code with."
  :type 'string
  :safe #'stringp)

(defcustom zig-test-optimization-mode "Debug"
  "Optimization mode to run tests with."
  :type 'string
  :safe #'stringp)


;; zig CLI commands

(defun zig--run-cmd (cmd &optional source &rest args)
  "Use compile command to execute a zig CMD with ARGS if given.
If given a SOURCE, execute the CMD on it."
  (let ((cmd-args (if source (cons source args) args)))
    (compilation-start (mapconcat 'shell-quote-argument
                                  `(,zig-zig-bin ,cmd ,@cmd-args) " "))))

;;;###autoload
(defun zig-compile ()
  "Compile using `zig build`."
  (interactive)
  (zig--run-cmd "build"))

;;;###autoload
(defun zig-build-exe ()
  "Create executable from source or object file."
  (interactive)
  (zig--run-cmd "build-exe" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-build-lib ()
  "Create library from source or assembly."
  (interactive)
  (zig--run-cmd "build-lib" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-build-obj ()
  "Create object from source or assembly."
  (interactive)
  (zig--run-cmd "build-obj" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-test-buffer ()
  "Test buffer using `zig test`."
  (interactive)
  (zig--run-cmd "test" (file-local-name (buffer-file-name)) "-O" zig-test-optimization-mode))

;;;###autoload
(defun zig-run ()
  "Create an executable from the current buffer and run it immediately."
  (interactive)
  (zig--run-cmd "run" (file-local-name (buffer-file-name)) "-O" zig-run-optimization-mode))

;;; Syntax table

(defvar zig-ts-mode--syntax-table
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
    (modify-syntax-entry ?/   ". 12"   table)
    (modify-syntax-entry ?*   "."      table)
    (modify-syntax-entry ?'   "\""     table)
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\\  "\\"     table)
    (modify-syntax-entry ?\n  ">"      table)
    table)
  "Syntax table for `rust-ts-mode'.")

;;; Indent

;; TODO: Adapt this to zig
;; most work ootb, but some are not needed.
(defvar zig-ts-mode--indent-rules
  `((zig
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "ContainerDecl") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "block") parent-bol zig-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `zig-ts-mode'.")

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
       :language 'zig
       :feature 'bracket
       '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

       :language 'zig
       :feature 'builtin
       '(([(BUILTINIDENTIFIER)]) @font-lock-builtin-face)

       :language 'zig
       :feature 'comment
       '(([(line_comment)]) @font-lock-comment-face)

       :language 'zig
       :feature 'delimiter
       '((["," "." ";" ":"]) @font-lock-delimiter-face)

       :language 'zig
       :feature 'keyword
       `([,@zig-ts-mode--keywords] @font-lock-keyword-face)

       :language 'zig
       :feature 'number
       '([(FLOAT) (INTEGER)] @font-lock-number-face)

       :language 'zig
       :feature 'function
       '((SuffixExpr
	  [
	   variable_type_function: (IDENTIFIER) @font-lock-function-call-face
	   field_constant: (IDENTIFIER) @font-lock-function-call-face])
	 (FieldOrFnCall
          [
	   field_access: (IDENTIFIER) @font-lock-function-call-face
	   function_call: (IDENTIFIER) @font-lock-function-call-face])
	 (FnProto
	  function: (IDENTIFIER) @font-lock-function-call-face))
       
       :language 'zig
       :feature 'assignment
       '((VarDecl variable_type_function: (_) @font-lock-variable-name-face)
	 (PtrListPayload variable: (IDENTIFIER) @font-lock-variable-name-face)
	 (Payload variable: (IDENTIFIER) @font-lock-variable-name-face))

       :language 'zig
       :feature 'operator
       `([,@zig-ts-mode--operators] @font-lock-operator-face)

       :language 'zig
       :feature 'string
       '([(STRINGLITERALSINGLE)] @font-lock-string-face)

       :language 'zig
       :feature 'type
       '([(BuildinTypeExpr)] @font-lock-type-face)

       :language 'zig
       :feature 'constant
       '(((CHAR_LITERAL) @font-lock-constant-face))

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

(defun zig-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("TestDecl"
     (treesit-node-text
      (treesit-node-child node 1) t))
    ("FnProto"
     (treesit-node-text
      (treesit-node-child-by-field-name node "function") t))))

;;; Mode definition

;; taken trom c-ts-mode.el
(defvar-keymap zig-ts-mode-map
  :doc "Keymap for Zig major mode with tree-sitter."
  :parent prog-mode-map
  "C-c C-b" #'zig-compile
  "C-c C-r" #'zig-run
  "C-c C-t" #'zig-test-buffer
  "C-c C-k" #'comment-region)

;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig"
  "Major mode for editing Zig, powered by tree-sitter.

\\{zig-ts-mode-map}"
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
                '(( comment )		; definitions
		  ( keyword string type )
		  ( assignment number builtin constant ) ; constants, literals
		  ( bracket operator function delimiter ))) ; delimiters, punctuation, functions properties variables


    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Test" "\\`TestDecl\\'" nil nil)
                  ("Fn" "\\`FnProto\\'" nil nil)))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules zig-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}():;,#" electric-indent-chars))

    ;; Navigation.
    ;; FIXME: Does not work for end-of-defun
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("TestDecl"
			      "FnProto")))
    (setq-local treesit-defun-name-function #'zig-ts-mode--defun-name)

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'zig)
    (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode)))

(provide 'zig-ts-mode)
;;; zig-ts-mode.el ends here
