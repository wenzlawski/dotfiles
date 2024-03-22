;;; svelte-ts-mode.el --- A treesitter mode for Svelte  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marc Wenzlawski

;; Author: Marc Wenzlawski <marcwenzlawski@gmail.com>
;; Keywords: languages

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

;; This package provides a treesitter mode for Svelte.

;;; Code:

(require 'treesit)
(require 'sgml-mode)

(defcustom svelte-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `svelte-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'svelte)

(defvar svelte-ts-mode--keywords
  '("as" "if" "each" "else")
  "TypeScript keywords for tree-sitter font-locking.")

(defvar svelte-ts-mode--operators
  '(":" "#" "/")
  "TypeScript operators for tree-sitter font-locking.")

(defvar svelte-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    (modify-syntax-entry ?\240 "."   table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?\^m "> b" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `svelte-ts-mode'.")

(defun svelte-ts-mode--indent-rules nil
  "Rules used for indentation."
  `((svelte
     ;; Note: in older grammars, `document' was known as
     ;; `fragment'.
     ((parent-is "document") parent-bol 0)
     ((node-is ,(regexp-opt '("element" "self_closing_tag"))) parent svelte-ts-mode-indent-offset)
     ((node-is "end_tag") parent 0)
     ((node-is "/") parent 0)
     ((parent-is "element") parent svelte-ts-mode-indent-offset)
     ((node-is "attribute") prev-sibling 0)
     ((node-is ">") parent 0)
     ((parent-is "start_tag") prev-sibling 0)
     ((parent-is "else_if_block") parent-bol svelte-ts-mode-indent-offset)
     ((node-is "raw_text") parent 0) 
     ((match "else_if_block" "if_statement") parent-bol 0)
     ((match "else_block" "if_statement") parent-bol 0)
     ((match "else_block" "else_if_block") parent-bol 0)
     ((match "else_if_block" "else_if_block") parent-bol 0)
     ((parent-is "if_statement") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "else_block") parent svelte-ts-mode-indent-offset)
     ((parent-is "each_statement") parent 0)
     ((node-is "text") parent 0)

     (no-node parent 0))))
;; ((node-is ,(regexp-opt '("element" "self_closing_tag"))) parent svelte-ts-mode-indent-offset)
;; ((node-is "end_tag") parent-bol 0)
;; ((node-is "/") parent-bol 0)
;; ((parent-is "element") parent-bol svelte-ts-mode-indent-offset)
;; ((parent-is "if_statement") parent-bol svelte-ts-mode-indent-offset)
;; ;;((parent-is "else_if_block") parent-bol svelte-ts-mode-indent-offset)
;; ((parent-is "else_block") parent-bol svelte-ts-mode-indent-offset)
;; ((parent-is "each_statement") parent-bol svelte-ts-mode-indent-offset)
;; ;;((node-is "text") parent-bol 0)
;; ;; ((node-is "attribute") prev-sibling 0)
;; ;; ((parent-is "attribute") prev-sibling 0)
;; ;;((node-is ">") parent 0)
;; ;; ((parent-is "start_tag") prev-sibling 0))))


(defun svelte-ts-mode--font-lock-settings nil
  "Tree-sitter font-lock rules for Svelte."
  (treesit-font-lock-rules
   :language 'svelte
   :feature 'delimiter
   '([ "<" ">" "/>" "</"] @font-lock-delimiter-face)

   :language 'svelte
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'svelte
   :feature 'attribute
   '((attribute (attribute_name)
		@font-lock-constant-face
		"=" @font-lock-bracket-face
		[(quoted_attribute_value) @font-lock-string-face
                 (expression (svelte_raw_text) @font-lock-constant-face)]))

   :language 'svelte
   :feature 'tag
   '((script_element
      [(start_tag (tag_name) @font-lock-doc-face)
       (end_tag (tag_name) @font-lock-doc-face)]))

   :language 'svelte
   :feature 'tag
   '([(start_tag (tag_name) @font-lock-function-call-face)
      (self_closing_tag (tag_name) @font-lock-function-call-face)
      (end_tag (tag_name)  @font-lock-function-call-face)])

   :language 'svelte
   :feature 'operator
   `([,@svelte-ts-mode--operators] @font-lock-operator-face)

   :language 'svelte
   :feature 'keyword
   `([,@svelte-ts-mode--keywords] @font-lock-keyword-face)

   :language 'svelte
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)
   ))

(defun svelte-ts-imenu-node-p (node)
  "Return t if NODE is a valid imenu node."
  (and (string-match-p "^h[0-6]$" (treesit-node-text node))
       (equal (treesit-node-type (treesit-node-parent node))
              "start_tag")))

(defun svelte-ts-imenu-name-function (node)
  "Return the name of the imenu entry for NODE."
  (let ((name (treesit-node-text node)))
    (if (svelte-ts-imenu-node-p node)
        (concat name " / "
                (thread-first (treesit-node-parent node)
                              (treesit-node-next-sibling)
                              (treesit-node-text)))
      name)))

(defun svelte-ts-setup ()
  "Setup for `svelte-ts-mode'."
  (interactive)

  ;; Indentation
  (setq-local treesit-simple-indent-rules (svelte-ts-mode--indent-rules))
  
  ;; Font-lock.
  (setq-local treesit-font-lock-settings
	      (svelte-ts-mode--font-lock-settings))
  ;; (apply #'treesit-font-lock-rules
  ;;        svelte-ts-font-lock-rules))
  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword constant tag attribute number)
                (declaration)
                (delimiter operator)))
  ;; (setq-local treesit-font-lock-feature-list
  ;;             '((comment declaration)
  ;;               (keyword string escape-sequence)
  ;;               (constant expression identifier number pattern property)
  ;;               (operator function bracket delimiter)))
  (setq-local treesit-font-lock-level 5)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              `(("Heading" svelte-ts-imenu-node-p nil svelte-ts-imenu-name-function)))

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode svelte-ts-mode sgml-mode "Svelte[ts]"
  "Major mode for editing Svelte."
  :group 'svelte
  :syntax-table svelte-ts-mode--syntax-table
  (when (treesit-ready-p 'svelte)
    (treesit-parser-create 'svelte)
    (svelte-ts-setup)))

(if (treesit-ready-p 'svelte)
    (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-ts-mode)))

(provide 'svelte-ts-mode)
;;; svelte-ts-mode.el ends here
