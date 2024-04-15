;; -*- lexical-binding: t; -*-

;; * BASIC SETTINGS

(setq warning-minimum-level :emergency)
(defun dir-concat (dir file)
  "join path DIR with filename FILE correctly"
  (concat (file-name-as-directory dir) file))
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29


;; decouple C-i and TAB
;; https://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab/221#221
(define-key input-decode-map [?\C-i] [C-i])

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-GUIX    (and IS-LINUX
                          (with-temp-buffer
                            (insert-file-contents "/etc/os-release")
                            (re-search-forward "ID=\\(?:guix\\|nixos\\)" nil t))))

;; Disable bidirectional text rendering for a modest performance boost. Just
;; need to remember to turn it on when displaying a right-to-left language!
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)
(when (> emacs-major-version 27)
  (setq redisplay-skip-fontification-on-input t))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Remove command line options that aren't relevant to our current OS; that
;; means less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp"))

(use-package server
  :defer 5
  :config
  (unless (server-running-p)
    (server-start)))

;; * PACKAGES

(require 'package)

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; NOTE 2023-08-21: I build Emacs from source, so I always get the
;; latest version of built-in packages.  However, this is a good
;; solution to set to non-nil if I ever switch to a stable release.
(setq package-install-upgrade-built-in nil)

;;(require 'use-package-ensure)
(setq use-package-always-ensure nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 f         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(straight-use-package '(org :type built-in))
;; (straight-use-package '(org))
(setq straight-use-package-by-default t)

;; * THEMES

(setq custom-safe-themes t)
(defalias 'my/apply-theme-change 'my/modus-theme-change)

(add-to-list 'ns-system-appearance-change-functions 'my/apply-theme-change)
;; (add-to-list 'after-make-frame-functions '(lambda (_)
;; (my/apply-theme-change ns-system-appearance))) ;; DOES NOT WORK
(push '(lambda (_) (my/apply-theme-change ns-system-appearance)) (cdr (last after-make-frame-functions)))
(use-package ef-themes)
(use-package color-theme-modern)

;; ** Modus themes

(defun my/theme-default-light ()
  "Set the default theme to light"
  (interactive)
  (load-theme 'modus-operandi t))

(defun my/theme-default-dark ()
  "Set the default theme to dark"
  (interactive)
  (load-theme 'modus-vivendi t))

(defun my/modus-theme-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (pcase appearance
    ('light (my/theme-default-light))
    ('dark  (my/theme-default-dark))))

(use-package modus-themes
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-mixed-fonts nil)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  (modus-themes-org-blocks nil)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts nil)
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 2.2))
     (agenda-date . (variable-pitch regular 1.3))))
  (modus-themes-custom-auto-reload t)
  :config
  (setopt modus-vivendi-palette-overrides
	  '((bg-main "#1A1A1A")
	    (bg-dim "#0E0E0E")
	    (fg-main "#E2E2E2")
	    (fg-dim "#999999")))
  (setopt modus-operandi-palette-overrides
	  '((bg-main "#F8F8F8")
	    (bg-dim "#EBEBEB")
	    (fg-main "#2C2C2C")
	    (fg-dim "#8B8B8B")))
  (setopt modus-themes-common-palette-overrides
	  '((cursor magenta-cooler)
	    (prose-done cyan-cooler)
	    (prose-tag fg-dim)
	    (modus-themes-completion-selected bg-dim)
	    (prose-table fg-main)
	    (bg-region bg-sage)
	    (fg-region unspecified)
	    (name blue-warmer)
	    (fg-heading-1 blue)
	    (fg-heading-2 olive)
	    (fg-heading-3 slate)
	    (fg-heading-4 maroon)
	    (fg-heading-5 olive)
	    (fg-heading-6 slate)
	    (fg-heading-7 maroon)
	    (fg-heading-8 olive)
	    (identifier magenta-faint)
	    (keybind magenta-cooler)
	    (accent-0 magenta-cooler)
	    (accent-1 cyan-cooler)
	    (accent-2 blue-warmer)
	    (accent-3 red-cooler)
	    (bg-completion bg-dim)
	    (bg-mode-line-active bg-dim)
	    (fg-mode-line-active fg-dim)
	    (bg-paren-match bg-magenta-intense)
	    (bg-mode-line-inactive bg-main)
	    (border-mode-line-active bg-mode-line-active)
	    (border-mode-line-inactive bg-mode-line-inactive)
	    (bg-tab-bar bg-main)
	    (bg-tab-current bg-active)
	    (bg-tab-other bg-main)
	    (prose-done green-faint)
            (prose-todo red-faint)
	    )))

;; ** Timu theme

(use-package timu-macos-theme
  :straight (:host github :repo "emacsmirror/timu-macos-theme")
  :init
  (setq timu-macos-flavour (symbol-name ns-system-appearance))
  :bind (:map help-map
	      ("t" . nil)
	      ("t s" . timu-macos-toggle-dark-light)))

(defun my/timu-theme-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (interactive)
  (customize-set-variable 'timu-macos-flavour (symbol-name appearance))
  (load-theme 'timu-macos t))

;; ** Poet theme

(use-package poet-theme
  :config
  (setq poet-theme-variable-pitch-multiplier 1.6)
  (setq poet-theme-variable-headers nil))

;; * USER INTERFACE
;; ** Writeroom

(defun my/writeroom-mode-hook ()
  "Custom behaviours for `writeroom-mode'."
  (if writeroom-mode
      (progn (centered-cursor-mode 1)
             (display-line-numbers-mode 0))
    (centered-cursor-mode 0)))

(use-package writeroom-mode
  :hook (writeroom-mode . my/writeroom-mode-hook))
(use-package centered-cursor-mode)

;; ** Spacious padding

(defun my/spacious-padding-reset ()
  "reset the spacious padding and modeline formats"
  (interactive)
  (spacious-padding-mode 1))

(use-package spacious-padding
  :config
  ;; (spacious-padding-mode)
  )

;; ** Pulsar

(use-package pulsar
  :hook
  (xref-after-return . pulsar-pulse-line)
  (xref-after-jump . pulsar-pulse-line)
  :custom
  (window-selection-change-functions '((lambda (_) (pulsar-pulse-line))))
  (window-buffer-change-functions '((lambda (_) (pulsar-pulse-line))))
  :custom-face
  (pulsar-green ((t (:background "#c0e7d4"))))
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
(remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

;; ** default-text-scale

(use-package default-text-scale
  :demand t
  :bind
  (:map default-text-scale-mode-map
        ("s-+" . default-text-scale-increase)
        ("s-_" . default-text-scale-decrease))
  :config
  (default-text-scale-mode))

;; ** rainbow-delimiters

(use-package rainbow-delimiters
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#D19A66"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#C678DD"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#56B6C2"))))
  ;; (rainbow-delimiters-depth-4-face ((t (:foreground "#F1CB02"))))
  ;; (rainbow-delimiters-depth-5-face ((t (:foreground "#F1CB02"))))
  ;; (rainbow-delimiters-depth-6-face ((t (:foreground "#F1CB02"))))
  ;; (rainbow-delimiters-depth-7-face ((t (:foreground "#F1CB02"))))
  ;; (rainbow-delimiters-depth-8-face ((t (:foreground "#F1CB02"))))
  ;; (rainbow-delimiters-depth-9-face ((t (:foreground "#F1CB02"))))
  :hook prog-mode
  :custom
  (rainbow-delimiters-max-face-count 3))

;; ** ns-auto-titlebar

(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode))
  (use-package osx-trash
    :config
    (osx-trash-setup)))

;; ** modeline

(require 'prot-modeline)
(defun prot-modeline-subtle-activate ()
  "Run prot-modeline-subtle-mode with 1"
  (interactive)
  (prot-modeline-subtle-mode 1))

(setq mode-line-compact nil) ; Emacs 28
;; write a function to do the spacing
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
    Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '((:eval
                 (simple-mode-line-render
                  (quote ("%e"
                          prot-modeline-kbd-macro
                          prot-modeline-narrow
                          prot-modeline-buffer-status
                          prot-modeline-input-method
                          prot-modeline-evil
                          prot-modeline-buffer-identification
                          "  "
                          prot-modeline-major-mode
                          prot-modeline-process
                          "  "
                          prot-modeline-vc-branch
                          "  "
                          prot-modeline-eglot
                          "  "
                          prot-modeline-flymake))
                  (quote (
                          " "
                          prot-modeline-misc-info
                          " "))))))
;;(prot-modeline-subtle-mode)

;; ** highlight visual line

(defun my/highlight-visual-line ()
  (save-excursion
    (cons (progn (beginning-of-visual-line) (point))
          (progn (end-of-visual-line) (point)))))
(setq hl-line-range-function 'my/highlight-visual-line)

;; ** hl-todo

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; * CONFIGURATION
;; ** user details

(setq user-full-name "Marc Wenzlawski"
      user-mail-address "marcwenzlawski@gmail.com")

;; (require 'secret)

;; ** secrets

(setq auth-sources '("~/.authinfo.gpg"))
(setopt epa-pinentry-mode 'loopback)
;; https://stackoverflow.com/questions/76388376/emacs-org-encrypt-entry-hangs-when-file-is-modified
;; DO NOT USE THIS WITH SYMMETRICALLY ENCRYPTED FILES.
;; MAY CAUSE FILE CORRUPTION.
(fset 'epg-wait-for-status 'ignore)

;; ** Emacs

(use-package emacs
  :straight nil
  :config
  (setq undo-limit 80000000)
  (setq auto-save-default t)
  (setq inhibit-compacting-font-caches t)
  (setq truncate-string-ellipsis "…")
  (setq shell-file-name (executable-find "fish"))
  (setq confirm-kill-emacs 'yes-or-no-p)
  (setq redisplay-dont-pause t)
  (setq-default line-spacing 0.1)
  (setq sentence-end-double-space nil)
  (setq require-final-newline t)
  (setq frame-inhibit-implied-resize t)
  (setq scroll-margin 0)
  (setq scroll-conservatively 0)
  ;;(setq frame-title-format '("" "what the %b"))
  (setq frame-title-format "\n")
  (setq ns-use-proxy-icon t)
  (setq cursor-type t)
  (setq blink-cursor-delay 1)
  (setq blink-cursor-interval 1)
  (setq register-preview-delay 0.25)
  (setq history-length 100)
  (setq initial-scratch-message ";; scratchy scratch")
  (setq visual-fill-column-center-text t)
  (setq-default fill-column 80)
  (setq prescient-history-length 1000)
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold nil)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq xref-search-program 'ripgrep)
  (setq delete-by-moving-to-trash t)
  (setq uniquify-buffer-name-style 'forward)
  (setq window-combination-resize t)
  (setq x-stretch-cursor t)
  (setq large-file-warning-threshold 100000000)
  ;; (setq exec-path (append exec-path '("~/.cargo/bin" "~/.pyenv/shims/")))
  (setq show-paren-delay 0)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (setq-default electric-indent-inhibit t)
  (pixel-scroll-precision-mode)
  (delete-selection-mode)
  (fringe-mode '(0 . 0))
  (blink-cursor-mode)
  (recentf-mode)
  (global-auto-revert-mode)
  (push '(lambda (_) (menu-bar-mode -1)) (cdr (last after-make-frame-functions)))
  (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-18"))
  (setq fit-window-to-buffer-horizontally t)
  (setq calendar-latitude '[50 50 north])
  (setq calendar-longitude '[12 55 east])
  :hook (prog-mode . show-paren-mode)
  :custom-face
  (show-paren-match ((t (:underline nil :inverse-video nil))))
  (variable-pitch ((t (:family "Open Sans"))))
  (fixed-pitch ((t (:family "Iosevka Comfy"))))
  :bind
  ("C-x C-l" . nil)
  ("C-x C-S-l" . downcase-region)
  ("C-c o" .  occur)
  ("C-x M-k" . kill-this-buffer)
  ("C-x <C-i>" . tab-to-tab-stop)
  ("C-<mouse-4>" . nil)
  ("C-<mouse-5>" . nil)
  ("C-<wheel-down>" . nil)
  ("C-<wheel-up>" . nil)
  ("C-c C" . calendar)
  (:map tab-prefix-map
        ("h" . tab-bar-mode)
        ("s" . tab-switcher))
  (:map help-map
        ("t" . nil)
        ("W" . man)
        ("t t" . consult-theme)
        ("t c" . centered-cursor-mode)
        ("t h" . hl-line-mode)
        ("t v" . variable-pitch-mode)
        ("t f" . visual-fill-column-mode)
        ("t l" . visual-line-mode)
        ("t t" . consult-theme)
        ("t c" . centered-cursor-mode)
        ("t h" . hl-line-mode)
        ("t v" . variable-pitch-mode)
        ("t f" . visual-fill-column-mode)
        ("t l" . visual-line-mode))
  ;;  (:map dired-mode-map
  ;;    ("K" . dired-kill-subdir))
  (:map completion-list-mode-map
        ("e" . switch-to-minibuffer)))
;; ** CUSTOM FILE

(use-package cus-edit
  :straight nil
  :config
  ;; Get custom-set-variables out of init.el
  (defvar my/custom-file (dir-concat user-emacs-directory "custom.el"))
  (setq custom-file my/custom-file)

  (defun my/cus-edit ()
    (let ((file my/custom-file))
      (unless (file-exists-p file)
        (make-empty-file file))
      (load-file file)))
  :hook (after-init . my/cus-edit))

(setq disabled-command-function nil)

;; Always start with *scratch*
;;(setq initial-buffer-choice t)


;; ** Man

(use-package man
  :straight nil
  :bind
  (:map Man-mode-map
	("g" . consult-imenu)))

;; ** editorconfig

(use-package editorconfig
  :config
  (editorconfig-mode))

;; ** dtrt-indent

(use-package dtrt-indent
  :config
  (setq dtrt-indent-verbosity 0))
;; (add-to-list 'dtrt-indent-hook-mapping-list '(lua-ts-mode lua lua-ts-indent-offset))
;; (dtrt-indent-global-mode))

;; ** gcmh

(use-package gcmh
  :config
  (gcmh-mode 1))

;; ** tabspaces

(use-package tabspaces)

;; ** outline

(use-package outline
  :custom
  (outline-minor-mode-prefix ""))

;; ** openwith

(use-package openwith)

;; ** tab-bar

(use-package tab-bar
  :custom
  (tab-bar-select-tab-modifiers '(super))
  :config
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; ** exec-path-from-shell

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; ** savehist

(use-package savehist
  :init
  (savehist-mode))

;; ** htmlize

(use-package htmlize)

;; ** noflet

(use-package noflet)

;; ** config profiler esup

(use-package esup)

;; * HELP
;; ** tldr

(use-package tldr
  :custom-face
  (tldr-command-itself ((t (:inherit font-lock-keyword-face :weight bold :background unspecified :foreground "orange"))))
  (tldr-command-argument ((t nil)))
  (tldr-code-block ((t (:foreground unspecified :background unspecified)))))

;; ** devdocs

(use-package devdocs
  :bind (:map help-map ("D" . devdocs-lookup)))

;; ** helpful

(use-package helpful
  :hook (helpful-mode . show-paren-local-mode)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ("C-h C-h" . helpful-at-point)
  ("C-h F" . helpful-function))

;; ** eldoc

(use-package eldoc
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  :config
  (setq eldoc-current-idle-delay 0.3))

;; *** yasnippet

(use-package yasnippet
  ;; (use-package yasnippet-snippets)
  ;; (setq yas-minor-mode-map
  ;;       (let ((map (make-sparse-keymap)))
  ;;         (define-key map (kbd "s") 'yas-insert-snippet)
  ;;         (define-key map (kbd "n") 'yas-new-snippet)
  ;;         (define-key map (kbd "v") 'yas-visit-snippet-file)
  ;;         map))
  ;; :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-verbosity 0)
  (yas-reload-all))

(use-package yankpad
  :disabled
  :init
  (setq yankpad-file "~/.emacs.d/yankpad.org"))

(use-package yasnippet-capf
  :disabled
  :straight (:host github :repo "elken/yasnippet-capf")
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))
;; :bind-keymap ("C-c s" . yas-minor-mode-map))

;; *** tempel

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  (:map tempel-map
	("<tab>" . tempel-next)
	("<backtab>" . tempel-previous))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
		      completion-at-point-functions)))
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  :config
  (defun tempel-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
	(message "Template %s not found" (cadr elt))
	nil)))
  (add-to-list 'tempel-user-elements #'tempel-include)

  (defun tempel-propmt (elt)
    (when (eq (car-safe elt) 'p)
      (if-let (prompt (alist-get (cadr elt) (tempel--templates)))
	  (cons 'l prompt))))

  :hook
  ((conf-mode prog-mode text-mode) . tempel-setup-capf))

(use-package tempel-collection
  :after tempel)

;; *** projectile

(use-package project)

(use-package projectile
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :bind
  (:map projectile-command-map ("b" . consult-project-buffer))
  :config
  (setq projectile-project-search-path
        '("~/fun/" "~/fun/web/" "~/fun/python" "~/fun/julia" "~/fun/projects" "~/dotfiles" "~/Dropbox/repos"))
  (projectile-global-mode 1))

;; *** quickrun

(use-package quickrun)

;; *** copilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook prog-mode
  :commands copilot-login
  :bind (:map copilot-completion-map ("<C-i>" . copilot-accept-completion))
  (:map help-map ("t C" . copilot-mode))
  :config
  (setq copilot-idle-delay 0.3))

(use-package jsonrpc)
;;  :pin gnu-elpa)

;; *** emmet

(use-package emmet-mode
  :commands (emmet-find-left-bound emmet-transform emmet-reposition-cursor)
  :hook (html-mode . emmet-mode))

;; *** pos-tip

(use-package pos-tip)

;; *** vterm

(use-package vterm
  :bind
  ("C-c t" . vterm)
  ("C-c 4 t" . vterm-other-window)
  :config
  (setq vterm-eval-cmds
	'(("find-file" find-file)
	  ("find-file-other-window" find-file-other-window)
          ("message" message)
          ("vterm-clear-scrollback" vterm-clear-scrollback)
          ("dired" dired)
	  ("man" man)
	  ("tldr" tldr)
          ("ediff-files" ediff-files)))
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell (executable-find "fish")))

;; * which-key

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h nil)
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

;; * USER EXPERIENCE
;; ** ace-window

(use-package ace-window
  :config
  (setq aw-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i))
  (setq aw-scope 'frame)
  (setq aw-reverse-frame-list t)
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?f aw-swap-window "Swap Windows")
          (?F aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?p aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help)))
  :bind
  ("C-x o" . ace-window)
  ("C-<tab>" . ace-window))

;; ** avy

(use-package avy
  :bind
  ("C-T" . avy-goto-char)
  ("C-t" . avy-goto-char-timer)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  :config
  (setq avy-background nil)
  (setq avy-dispatch-alist
        '((?x . avy-action-kill-move)
          (?X . avy-action-kill-stay)
          (?h . avy-action-teleport)
          (?u . avy-action-mark)
          (?c . avy-action-copy)
          (?y . avy-action-yank)
          (?f . avy-action-ispell)
          (?z . avy-action-zap-to-char)))
  (setq avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o)))

;; ** embark

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :after consult embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package dash)
(use-package embark-vc)

;; ** hydra

(use-package hydra
  :bind
  (:map outline-minor-mode-map
	("C-c <tab>" . hydra-outline/body))
  :config
  (use-package major-mode-hydra)
  
  (with-eval-after-load 'outline
    (defhydra hydra-outline (:color pink :hint nil)
      "
^Hide^             ^Show^           ^Move                      ^Edit
^^^^^^----------------------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up                    _U_: up
_t_: body          _e_: entry       _n_: next visible          _D_: down
_o_: other         _i_: children    _p_: previous visible      _<_: promote
_c_: entry         _k_: branches    _f_: forward same level    _>_: demote
_l_: leaves        _s_: subtree     _b_: backward same level   
_d_: subtree                      _/_: outline
"
      ;; Hide
      ("q" outline-hide-sublevels)    ; Hide everything but the top-level headings
      ("t" outline-hide-body)         ; Hide everything but headings (all body lines)
      ("o" outline-hide-other)        ; Hide other branches
      ("c" outline-hide-entry)        ; Hide this entry's body
      ("l" outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
      ("d" outline-hide-subtree)      ; Hide everything in this entry and sub-entries
      ;; Show
      ("a" outline-show-all)          ; Show (expand) everything
      ("e" outline-show-entry)        ; Show this heading's body
      ("i" outline-show-children)     ; Show this heading's immediate child sub-headings
      ("k" outline-show-branches)     ; Show all sub-headings under this heading
      ("s" outline-show-subtree)      ; Show (expand) everything in this heading & below
      ;; Move
      ("u" outline-up-heading)                ; Up
      ("n" outline-next-visible-heading)      ; Next
      ("p" outline-previous-visible-heading)  ; Previous
      ("f" outline-forward-same-level)        ; Forward - same level
      ("b" outline-backward-same-level)       ; Backward - same level
      ("/" consult-outline)
      ;; Edit
      ("k" outline-headers-as-kill)        ; Kill this heading
      ("U" outline-move-subtree-up)        ; Up
      ("D" outline-move-subtree-down)
      ("<" outline-promote)           ; Promote
      (">" outline-demote)            ; Demote
      ("z" nil "leave")))	      ; end hydra

  (with-eval-after-load 'anki-helper
    ))




;; ** undo-fu

(use-package undo-fu)

(use-package undo-fu-session)

;; ** vundo

(use-package vundo)

;; ** transpose-frame

(use-package transpose-frame
  :straight (:host github :repo "emacsorphanage/transpose-frame")
  :bind
  ("C-x 4 t" . transpose-frame)
  ("C-x 4 i" . flip-frame)
  ("C-x 4 o" . flop-frame)
  ("C-x 4 n" . rotate-frame))

;; ** hide-mode-line

(use-package hide-mode-line
  :bind (:map help-map ("t m" . hide-mode-line-mode)))

;; ** bookmark+

(use-package bookmark+
  :straight (bookmark+))

(setq bookmark-save-flag 1)

;; ** scratch

(use-package scratch
  :straight (:host codeberg :repo "emacs-weirdware/scratch" :files ("*.el")))

;; ** exiftool

(use-package exiftool
  :defer t)

;; ** pandoc-mode

(use-package pandoc-mode
  :hook ((text-mode doc-view-mode pdf-view-mode) . pandoc-mode)
  :bind (:map pandoc-mode-map
	      ("C-c p" . pandoc-main-hydra/body)
	      ("C-c /" . nil)))

;; ** corfu

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto nil)               ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator) ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; ** abbrev

(use-package abbrev
  :straight nil
  :config
  (setq save-abbrevs 'silently)
  (quietly-read-abbrev-file))

;; ** dabbrev

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :straight nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; ** cape

(use-package cape)

;; ** expand-region

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))
;; ** completion

(defun my/sort-by-length (elements)
  "Sort ELEMENTS by minibuffer history, else return them unsorted.
This function can be used as the value of the user option
`completions-sort'."
  (sort
   elements
   (lambda (c1 c2) (< (length c1) (length c2)))))

(setq completions-format 'one-column)
(setq completion-show-help nil)
(setq completion-auto-help t)
(setq completion-auto-select t)
(setq completions-detailed nil)
(setq completion-show-inline-help nil)
(setq completions-max-height 15)
(setq completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face))
;;(bind-key "e" #'switch-to-minibuffer 'completion-list-mode-map)
;;(bind-key "<return>" #'minibuffer-force-complete-and-exit 'minibuffer-mode-map)
;;(bind-key "C-<return>" #'minibuffer-tcomplete-and-exit 'minibuffer-mode-map)

;; ** marginalia

(use-package marginalia
  :init
  (marginalia-mode))

;; ** orderless

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ** vertico

;; Adapted from vertico-reverse
(defun vertico-bottom--display-candidates (lines)
  "Display LINES in bottom."
  (move-overlay vertico--candidates-ov (point-min) (point-min))
  (unless (eq vertico-resize t)
    (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put vertico--candidates-ov 'before-string string)
    (overlay-put vertico--candidates-ov 'after-string nil))
  (vertico--resize-window (length lines)))

;; Enable vertico
(use-package vertico
  :bind (:map vertico-map
	      ("C-c C-n" . vertico-quick-jump))
  :custom-face
  ;;(vertico-current ((t (:background "slate"))))
  :init (vertico-mode)
  ;;(advice-add #'vertico--display-candidates :override #'vertico-bottom--display-candidates)
  (setq vertico-scroll-margin 0)       ;; Different scroll margin
  (setq vertico-count 10)
  (setq vertico-resize 'grow-only))
;;(setq vertico-count-format '("" . "")))

(use-package vertico-multiform
  :straight nil
  :after vertico
  :init
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-buffer flat))))

;; ** consult

(use-package consult
  :after org
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-o" . consult-outline)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s R" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  (:map project-prefix-map
        ("b" . consult-project-buffer))
  (:map isearch-mode-map
        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch
  (:map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history))
  (:map org-mode-map
        ("C-c h" . consult-org-heading)))

;; *** consult-flycheck

(use-package consult-flycheck)

;; *** consult-frecoll

(use-package consult-recoll
  :after citar
  :config
  ;; (setq exec-path (append exec-path '("/usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS/")))
  (consult-recoll-embark-setup))

;; *** consult-notes

(use-package consult-notes
  :after consult denote
  :demand t
  :custom-face
  (consult-notes-sep ((t (:foreground "CornFlowerBlue"))))
  :bind
  ("C-c n o" . consult-notes)
  ("C-c n X" . consult-notes-search-in-all-notes)
  ("C-c n 4 o" . my/consult-notes-other-window)
  :config
  (consult-customize consult-notes my/consult-notes-other-window :preview-key "M-.")

  (setq consult-notes-file-dir-sources
        '(("Org"             ?o "~/Dropbox/Org/")))
  (setq consult-notes-org-headings-files '("~/Dropbox/Org/"))
  ;;(consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (setq consult-notes-denote-display-id nil)
    (consult-notes-denote-mode))

  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files))
  (setq consult-notes-denote-truncate-title 60))

(with-eval-after-load 'consult-notes-denote
  (defvar consult-notes-denote-truncate-title nil
    "Truncate title in Denote notes. Can be nil or a number.")
  
  (defconst consult-notes-denote--source
    (list :name     (propertize "Denote notes" 'face 'consult-notes-sep)
          :narrow   ?d
          :category consult-notes-category
          :annotate consult-notes-denote-annotate-function
          :items    (lambda ()
                      (let* ((max-width (if consult-notes-denote-truncate-title consult-notes-denote-truncate-title 0))
                             (cands (mapcar (lambda (f)
                                              (let* ((id (denote-retrieve-filename-identifier f))
                                                     (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f)) (denote-retrieve-filename-title f)))
                                                     (title (if consult-notes-denote-display-id
								(concat id " " title-1)
                                                              title-1))
						     (title (if consult-notes-denote-truncate-title
								(truncate-string-to-width title consult-notes-denote-truncate-title) title))
                                                     (dir (file-relative-name (file-name-directory f) denote-directory))
                                                     (keywords (denote-extract-keywords-from-path f)))
						(if (not consult-notes-denote-truncate-title)
						    (let ((current-width (string-width title)))
                                                      (when (> current-width max-width)
							(setq max-width current-width))))
						(propertize title 'denote-path f 'denote-keywords keywords)))
                                            (funcall consult-notes-denote-files-function))))
			(mapcar (lambda (c)
                                  (let* ((keywords (get-text-property 0 'denote-keywords c))
					 (path (get-text-property 0 'denote-path c))
					 (dirs (directory-file-name (file-relative-name (file-name-directory path) denote-directory))))
                                    (concat c
                                            ;; align keywords
                                            (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
                                            (format "%18s"
                                                    (if keywords
							(concat (propertize "#" 'face 'consult-notes-name)
								(propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
                                                      ""))
                                            (when consult-notes-denote-dir (format "%18s" (propertize (concat "/" dirs) 'face 'consult-notes-name))))))
				cands)))
          ;; Custom preview
          :state  #'consult-notes-denote--state
          ;; Create new note on match fail
          :new     #'consult-notes-denote--new-note)))

(defun consult-notes-my-embark-function (cand)
  "Do something with CAND"
  (interactive "fNote: ")
  (message cand))

(defun my/consult-notes--on-file (file)
  (let ((consult-notes-org-headings-files (list file)))
    (consult-notes)))

(defun my/consult-notes--menu ()
  (let* ((curr-buf buffer-file-name)
         (avy-keys '(?a ?p ?b ?r ?f ?j ?c ?t))
         (file (avy-menu "*select notes*"
                         '("Select file"
                           (""
                            ("All"      . "~/Dropbox/Org/")
                            ("People"   . "~/Dropbox/Org/people.org")
                            ("Books"    . "~/Dropbox/Org/books.org")
                            ("Refile"   . "~/Dropbox/Org/refile.org")
                            ("Projects" . "~/Dropbox/Org/projects.org")
                            ("Config"  . "~/.config/doom/config.org")
                            ("This file" . curr-buf))))))
    (if (symbolp file)
        (eval file))
    file))

(defun my/consult-notes-org-insert-link ()
  (interactive)
  (let ((file (my/consult-notes--menu)))
    (if (stringp file)
        (progn (if (equal major-mode 'org-mode)
                   (progn (org-mark-ring-push)
                          (my/consult-notes--on-file file)
                          (org-store-link nil t)
                          (org-mark-ring-goto)
                          (org-insert-all-links nil "" " "))
                 (my/consult-notes--on-file file))))))


(defun my/consult-notes-menu ()
  (interactive)
  (let ((file (my/consult-notes--menu)))
    (if (stringp file)
        (progn
          (if (equal major-mode 'org-mode) (org-mark-ring-push))
          (my/consult-notes--on-file file)
          (org-narrow-to-subtree)
          (org-fold-show-subtree)))))

(defun my/consult-notes-other-window ()
  "Open a note in another window"
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-notes)))

;; *** consult-flyspell

(use-package consult-flyspell
  :bind (:map flyspell-mode-map ("C-<" . consult-flyspell))
  :config
  ;; default settings
  (setq consult-flyspell-select-function 'flyspell-correct-at-point
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

;; ** grep

(use-package grep
  :config
  (when (executable-find "rg")
    (setq grep-command "rg --no-heading --line-number --color never %s %s")
    (setq grep-program "rg")))

;; ** rg

(use-package rg
  :config
  (rg-define-search search-denote
    "Search files including hidden in home directory"
    :query ask
    :format literal
    :files "*.org"
    :dir denote-directory
    :menu ("Search" "n" "Denote"))
  :bind
  ("M-s r" . rg-menu)
  ("C-c n f R" . search-denote)
  (:map isearch-mode-map
        ("M-s g" . rg-isearch-menu)))

;; ** wgrep

(use-package wgrep)

;; ** substitute

(use-package substitute)

;; ** occur-x

(use-package occur-x
  :hook (occur-mode . turn-on-occur-x-mode))

;; ** loccur

(use-package loccur
  :straight (:host codeberg :repo "fourier/loccur")
  :bind
  (:map isearch-mode-map
        ("M-s l" . loccus-isearch)))

;; ** dired

(use-package dired
  :straight nil
  :bind
  (:map dired-mode-map
	("M-o" . dired-do-open)
	("M-RET" . my/open-current-dir-in-finder))
  :config
  (defun my/open-current-dir-in-finder ()
    "Open current directory in Finder."
    (interactive)
    (shell-command "open .")))
(use-package dired-x
  :straight nil
  :after dired)
(use-package dired-hacks-utils
  :after dired
  :config
  (use-package dired-subtree))
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode))
(use-package fd-dired)


;; ** magit / git

(use-package magit)

(use-package diff-hl)

(use-package git-gutter)

(use-package vc-git
  :straight nil
  :config
  (autoload 'vc-git-root "vc-git"))

;; * LANGUAGE TOOLS
;; ** eglot

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((svelte-mode svelte-ts-mode) . ("svelteserver" "--stdio")))
  :bind
  (:map eglot-mode-map
	("C-c e f" . eglot-format)
        ("C-c e q" . eglot-shutdown)
        ("C-c e Q" . eglot-shutdown-all)
        ("C-c e l" . eglot-list-connections)
	("C-c e r" . eglot-rename))
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit highlight)))))

;; ** flymake

(use-package flymake
  ;;:pin gnu-elpa
  :bind
  (:map flymake-mode-map
	("C-c M-n" . flymake-goto-next-error)
	("C-c M-p" . flymake-goto-prev-error)
	("C-c M-l" . flymake-show-project-diagnostics)))

;; ** flycheck flyspell

(use-package flycheck)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :after flyspell-correct)

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        (if (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode))
          ;; else
          (progn
            (message "Flyspell on (text)")
            (flyspell-mode 1)))
        ;; I tried putting (flyspell-buffer) here but it didn't seem to work
        )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
                                        ; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;; not being used, as we are not using ispell dicts
(defun my/switch-dictionary()
  "UNUSED. Toggle dictionary language between english and german"
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(use-package flyspell
  :bind (:map help-map ("t s" . flyspell-toggle))
  :config
  (cond
   ;; try hunspell at first
   ;; if hunspell does NOT exist, use aspell
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          flyspell-issue-message-flag nil
          ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
          ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,de_DE_frami") nil utf-8)))

    ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
    ;; If it's nil, Emacs tries to automatically set up the dictionaries.
    (when (boundp 'ispell-hunspell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))))

;; ** flymake ruff

(use-package flymake-ruff
  :after flymake
  :hook ((python-mode python-ts-mode) . flymake-ruff-load))

;; ** tree-sitter

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (regex "https://github.com/tree-sitter/tree-sitter-regex")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (r "https://github.com/r-lib/tree-sitter-r")
        (svelte "https://github.com/tree-sitter-grammars/tree-sitter-svelte")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
		    "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master"
             "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
		    "master" "typescript/src")
        (typst "https://github.com/uben0/tree-sitter-typst")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
(setq treesit-font-lock-level 4)

(use-package treesit-auto
  :disabled

  :config
  (global-treesit-auto-mode))

;; ** apheleia

(use-package apheleia
  :bind
  (:map prog-mode-map ("C-c f" . apheleia-format-buffer))
  :config

  (with-eval-after-load 'julia-mode
    (push
     '(julia . ((dir-concat user-emacs-directory "scripts/julia-format.sh") inplace ))
     apheleia-formatters)
    (add-to-list 'apheleia-mode-alist '(julia-mode . julia)))

  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))

  (apheleia-global-mode))

;; ** format-all

;; problem with emacs format region
;; but apheleia does not have format region.
(use-package format-all
  :disabled)

;; * LANGUAGE MODES
;; ** lisp

(use-package lisp-mode
  :straight nil
  :hook (lisp-data-mode . electric-pair-mode))

;; ** elisp

(use-package elisp-mode
  :straight nil
  :hook (emacs-lisp-mode . electric-pair-mode)
  :bind
  (:map emacs-lisp-mode-map
	("M-i" . completion-at-point))
  (:map lisp-interaction-mode-map
	("M-i" . completion-at-point)))

;; ** python

(use-package python
  :bind
  (:map python-ts-mode-map
	("C-c M-e" . eglot)
	("M-i" . completion-at-point)
	("M-o" . consult-imenu))
  :config
  (use-package pyenv-mode
    :init
    (setq pyenv-mode-map
          (let ((map (make-sparse-keymap)))
            map))
    :hook (python-ts-mode .  pyenv-mode)
    :bind
    (:map python-ts-mode-map
          ("C-c C-s" . pyenv-mode-set)
          ("C-c C-u" . pyenv-mode-unset)))
  (use-package poetry
    :bind
    (:map python-ts-mode-map
          ("C-c C-b" . poetry)))
  (use-package python-pytest
    :bind
    (:map python-ts-mode-map
          ("C-c C-n" . python-pytest-dispatch))))

;; ** ein

(use-package ein
  :config)

(use-package ob-ein
  :straight nil
  :after ein python-ts-mode
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((ein . t))))

;; ** ess

(use-package ess
  :defer t
  :config
  (setq ess-eval-visibly 'nowait)
  (setq ess-use-company 'nil)
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op% . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t))))

;; (setq display-buffer-alist
;;       '(("*R Dired"
;;          (display-buffer-reuse-window display-buffer-at-bottom)
;;          (window-width . 0.5)
;;          (window-height . 0.25)
;;          (reusable-frames . nil))
;;         ("*R"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . -1)
;;          (window-width . 0.5)
;;          (reusable-frames . nil))
;;         ("*Help"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . 1)
;;          (window-width . 0.5)
;;          (reusable-frames . nil))))

(defvar rutils-show_plot_next_to_r_process t)

(defun add-pdf-to-rcode(rcomm fname)
  "add pdf(tmpfile) and dev.off() to R command"
  (let*  (
          (newc (concat "pdf('" fname "')\n" rcomm  "\n dev.off()"))
          )
    (eval newc)
    )
  )

(defun rutils-plot-region-or-paragraph()
  "execute region or paragraph and save tmp plot to pdf. Then open windows to show pdf"
  (interactive)
  (let*  (
          (fname (concat (make-temp-file "plot_") ".pdf"))
          )
    (progn
      (if (use-region-p)
          (ess-eval-linewise (add-pdf-to-rcode (buffer-substring (region-beginning) (region-end)) fname))
        (progn (ess-eval-linewise (add-pdf-to-rcode (thing-at-point 'paragraph) fname)))
        )
      ;; (with-help-window "*plots*"
      ;;   (find-ssfile-at-point)
      ;;   )
      (if rutils-show_plot_next_to_r_process
          (ess-switch-to-end-of-ESS)
        )
      (if (window-in-direction 'below)
          (progn
            (select-window (window-in-direction 'below))
            (find-file fname)
            )
        (progn
          (split-window-below)
          (select-window (window-in-direction 'below))
          (find-file fname)
          )
        )
      ;;(split-window-right)
      ;;(windmove-right)
      )
    )
  )

;; ** julia

(use-package julia-mode
  :hook (julia-mode . (lambda nil (progn (apheleia-mode -1) (setq-local eglot-connect-timeout 300))))
  :mode "\\.jl\\'")

(use-package julia-snail
  :bind (:map julia-snail-mode-map ("C-c f" . julia-snail/formatter-format-buffer))
  :custom
  (julia-snail-popup-display-eval-results nil)
  (julia-snail-repl-display-eval-results t)
  (julia-snail-multimedia-enable t)
  (julia-snail-extensions '(repl-history formatter))
  :config
  (add-to-list 'display-buffer-alist
	       '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))
  :hook (julia-mode . julia-snail-mode))

(use-package eglot-jl
  :after eglot
  :custom
  (eglot-jl-language-server-project "~/.julia/environments/v1.10")
  :config
  (eglot-jl-init))

;; ** markdown

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))
;; ** typst

(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :custom
  (typst-ts-mode-watch-options "--open"))

;; ** fish

(use-package fish-mode
  :config
  (setq fish-enable-auto-indent t))
;; ** lua

(use-package lua-mode
  :config
  (setq lua-indent-level 3))
(use-package lua-ts-mode
  :hook (lua-ts-mode . (lambda nil (setq tab-width 3)))
  :config
  (setq lua-ts-indent-offset 3)
  :straight (:host sourcehut :repo "johnmuhl/lua-ts-mode" :files ("*.el")))

;; ** cc-mode

(use-package cc-mode
  :hook (awk-mode . (lambda nil (setq tab-width 4))))

;; ** css

(use-package css-mode
  :hook ((css-mode css-ts-mode) . (lambda nil (setq tab-width 2)))
  :config
  (setq css-indent-offset 2))

;; ** nix

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-buffer)

;; * ORG

(require 'setup-org)


;; * APPLICATIONS
;; ** elfeed

(use-package elfeed
  :disabled)
(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/feeds.org"))
  (setq elfeed-search-title-max-width 100)
  (elfeed-org))

;; ** notmuch

(use-package notmuch)

;; ** eww

(defun my/scroll-up-half ()
  (interactive)
  (scroll-up-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(defun my/scroll-down-half ()
  (interactive)
  (scroll-down-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(use-package eww
  :bind
  ("C-c w" . eww)
  (:map eww-mode-map
	("D" . eww-download)
	("d" . my/scroll-up-half)
	("u" . my/scroll-down-half)
	("U" . eww-up-url))
  :config
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)

  (defun eww-browse-with-external-browser (&optional url)
    "Browse the current URL with an external browser.
The browser to used is specified by the
`browse-url-secondary-browser-function' variable."
    (interactive nil eww-mode)
    (let ((url (or url (plist-get eww-data :url))))
      (if (s-starts-with? "https://html.duckduckgo.com/html/" url)
	  (setq url (concat "https://duckduckgo.com/?q=" (cadar (url-parse-query-string url)))))
      (funcall browse-url-secondary-browser-function
               (or url (plist-get eww-data :url)))))
  )

(use-package shr
  :bind
  (:map shr-map
	("u" . nil)))

;; ** shr

(use-package shr
  :config
  (setq shr-max-image-proportion 0.4))

;; ** TODO hyperbole

(use-package hyperbole
  :disabled
  :bind
  (:map hyperbole-mode-map
	("M-o" . nil)
	("ESC <return>" . nil))
  :config
  (setq hbmap:dir-user "~/.emacs.d/hyperb")
  (hyperbole-mode 1))

;; ** denote

(defun my/denote-rename-buffer ()
  (interactive)
  (denote-rename-buffer))

(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/Library/CloudStorage/Dropbox/denote"))
  (setq denote-dired-directories '("~/Library/CloudStorage/Dropbox/denote"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-rename-no-confirm t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-date-format nil)
  (setq denote-backlinks-show-context t)
  (denote-rename-buffer-mode 1)
  :hook (dired-mode-hook . denote-dired-mode-in-directories)
  :bind
  ("C-c n C-r" . my/denote-rename-buffer)
  ("C-c n I" . denote-add-links)
  ("C-c n L" . denote-link-or-create)
  ("C-c n N" . denote-type)
  ("C-c n R" . denote-rename-file-using-front-matter)
  ("C-c n b" . denote-backlinks)
  ("C-c n d" . denote-date)
  ("C-c n h" . denote-org-extras-link-to-heading)
  ("C-c n f b" . denote-find-backlink)
  ("C-c n f f" . denote-find-link)
  ("C-c n f r" . my/denote-rg-search)
  ("C-c n i" . denote-link) ; "insert" mnemonic
  ("C-c n l" . denote-link-after-creating)
  ("C-c n n" . denote)
  ("C-c n p" . denote-region) ; "contents" mnemonic
  ("C-c n P" . denote-org-extras-extract-org-subtree)

  ("C-c n r" . denote-rename-file)
  ("C-c n s" . denote-subdirectory)
  ("C-c n t" . denote-template)
  ("C-c n z" . denote-signature)) ; "zettelkasten" mnemonic

(use-package denote-org-extras
  :straight nil
  :after denote)


(defun my/denote-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep denote-directory)))

(use-package denote-explore
  :custom
  ;; Where to store network data and in which format
  ;; (denote-explore-network-directory "<folder>")
  ;; (denote-explore-network-filename "<filename?")
  (denote-explore-network-format 'graphviz)
  :bind
  (;; Statistics
   ("C-c n e c" . denote-explore-count-notes)
   ("C-c n e C" . denote-explore-count-keywords)
   ("C-c n e b" . denote-explore-keywords-barchart)
   ("C-c n e x" . denote-explore-extensions-barchart)
   ;; Random walks
   ("C-c n e r" . denote-explore-random-note)
   ("C-c n e l" . denote-explore-random-link)
   ("C-c n e k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c n e d" . denote-explore-identify-duplicate-notes)
   ("C-c n e z" . denote-explore-zero-keywords)
   ("C-c n e s" . denote-explore-single-keywords)
   ("C-c n e o" . denote-explore-sort-keywords)
   ("C-c n e r" . denote-explore-rename-keywords)
   ;; Visualise denote
   ("C-c n e n" . denote-explore-network)
   ("C-c n e v" . denote-explore-network-regenerate)
   ("C-c n e D" . denote-explore-degree-barchart)))


;; ** nov-mode

(defun my/center-reading-mode ()
  "Center the text in visual column mode"
  (interactive)
  (visual-fill-column-mode))

;; TODO make this respeatable, and work with n argument
(defun my/mark-whole-sentence ()
  "Mark the whole sentence the cursor is in."
  (interactive)
  (backward-sentence)
  (mark-end-of-sentence nil))

;; (defun my/nov-font-setup ()
;;   (face-remap-add-relative 'variable-pitch :family "ETBembo"))

(defun my/nov-mode-setup ()
  "Set up the nov mode"
  ;; (my/nov-font-setup)
  (hl-line-mode -1)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (variable-pitch-mode 1))

(defun my/toggle-header-line ()
  "Toggle the display of the header line"
  (interactive)
  (if nov-header-line-format
      (setq nov-header-line-format nil)
    (setq nov-header-line-format "%t: %c"))
  (nov-render-document))

(defun my/toggle-cursor-display ()
  "Toggle between displaying a bar and no cursor"
  (interactive)
  (if cursor-type
      (setq cursor-type nil)
    (setq cursor-type 'bar)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  :bind
  (:map nov-mode-map
	("j" . (lambda () (interactive) (scroll-up 1)))
	("k" . (lambda () (interactive) (scroll-down 1)))
	("z" . visual-fill-column-mode)
	("d" . +lookup/dictionary-definition)
	("m" . nil)
	("h" . nil)
	("y" . org-store-link)
	("m p" . mark-paragraph)
	("m s" . my/mark-whole-sentence)
	("h m" . org-remark-mark)
	("h l" . org-remark-mark-line)
	("h o" . org-remark-open)
	("h n" . org-remark-next)
	("h p" . org-remark-prev)
	("h ]" . org-remark-view-next)
	("h [" . org-remark-view-prev)
	("h r" . org-remark-remove)
	("h d" . org-remark-delete)
	("h v" . org-remark-view)
	("h q" . delete-other-windows)
	("C-c t" . my/toggle-header-line)
	("C-c v" . visual-line-mode)
	("C-c c" . my/toggle-cursor-display)
	("C-c b" . org-noter))
  :hook (nov-mode . my/nov-mode-setup))

;; ** esxml

(use-package esxml)

;; ** calibredb

(defun my/refresh-calibre-bib ()
  (interactive)
  (shell-command "calibredb catalog /tmp/cat.bib --fields=title,authors,formats,id,isbn,pubdate,tags,uuid,identifiers" )
  (shell-command "awk -f ~/.emacs.d/scripts/escape_comma.awk /tmp/cat.bib > ~/cat.bib"))

(use-package calibredb
  :bind
  ("C-c d" . calibredb)
  ("C-c D" . calibredb-consult-read)
  :config
  (setq calibredb-root-dir "~/Dropbox/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-id-width 5)
  (setq calibredb-title-width 55)
  (setq calibredb-preferred-format 'pdf)
  (setq calibredb-library-alist '(("~/Dropbox/Calibre Library"))))

(with-eval-after-load 'calibredb
  (defun calibredb-all-author-sort nil "Get all author-sort and return as a list."
	 (seq-uniq
	  (let
	      (l)
	    (let*
		((--cl-var-- calibredb-full-entries)
		 (entry nil))
	      (while
		  (consp --cl-var--)
		(setq entry
		      (car --cl-var--))
		(setq l
		      (append
		       (split-string
			(calibredb-getattr
			 (cdr entry)
			 :author-sort)
			"&" t "\s+")
		       l))
		(setq --cl-var--
		      (cdr --cl-var--)))
	      nil)
	    l)))

  (defun calibredb-format-item (book-alist)
    "Format the candidate string shown in helm or ivy.
Argument BOOK-ALIST ."
    (let ((id (calibredb-getattr (list book-alist) :id))
	  (title (calibredb-getattr (list book-alist) :book-title))
	  (format (calibredb-getattr (list book-alist) :book-format))
	  (author (calibredb-getattr (list book-alist) :author-sort))
	  (tag (calibredb-getattr (list book-alist) :tag))
	  (comment (calibredb-getattr (list book-alist) :comment))
	  (size (calibredb-getattr (list book-alist) :size))
	  (ids (calibredb-getattr (list book-alist) :ids))
	  (date (calibredb-getattr (list book-alist) :last_modified))
	  (favorite-map (make-sparse-keymap))
	  (tag-map (make-sparse-keymap))
	  (format-map (make-sparse-keymap))
	  (author-map (make-sparse-keymap))
	  (date-map (make-sparse-keymap)))
      (define-key favorite-map [mouse-1] 'calibredb-favorite-mouse-1)
      (define-key tag-map [mouse-1] 'calibredb-tag-mouse-1)
      (define-key format-map [mouse-1] 'calibredb-format-mouse-1)
      (define-key author-map [mouse-1] 'calibredb-author-mouse-1)
      (define-key date-map [mouse-1] 'calibredb-date-mouse-1)
      (if calibredb-detailed-view
	  (setq title (concat title "\n")))
      (format
       (if calibredb-detailed-view
	   (let ((num (cond (calibredb-format-all-the-icons 3)
			    (calibredb-format-icons-in-terminal 3)
			    ((>= calibredb-id-width 0) calibredb-id-width)
			    (t 0 ))))
	     (concat
	      "%s%s%s"
	      (calibredb-format-column (format "%sFormat:" (make-string num ? )) (+ 8 num) :left) "%s\n"
	      (calibredb-format-column (format "%sDate:" (make-string num ? )) (+ 8 num) :left) "%s\n"
	      (calibredb-format-column (format "%sAuthor:" (make-string num ? ))  (+ 8 num) :left) "%s\n"
	      (calibredb-format-column (format "%sTag:" (make-string num ? )) (+ 8 num) :left) "%s\n"
	      (calibredb-format-column (format "%sIds:" (make-string num ? )) (+ 8 num) :left) "%s\n"
	      (calibredb-format-column (format "%sComment:" (make-string num ? )) (+ 8 num) :left) "%s\n"
	      (calibredb-format-column (format "%sSize:" (make-string num ? )) (+ 8 num) :left) "%s"))
	 "%s%s%s %s %s %s (%s) %s %s %s")
       (cond (calibredb-format-all-the-icons
	      (concat (if (fboundp 'all-the-icons-icon-for-file)
			  (all-the-icons-icon-for-file (calibredb-get-file-path (list book-alist))) "")
		      " "))
	     (calibredb-format-icons-in-terminal
	      (concat (if (fboundp 'icons-in-terminal-icon-for-file)
			  (icons-in-terminal-icon-for-file (calibredb-get-file-path (list book-alist) ) :v-adjust 0 :height 1) "")
		      " "))
	     (calibredb-format-character-icons
	      (concat (calibredb-attach-icon-for (calibredb-get-file-path (list book-alist))) " "))
	     (t ""))
       (calibredb-format-column (format "%s" (propertize id 'face 'calibredb-id-face 'id id)) calibredb-id-width :left)
       (calibredb-format-column (format "%s%s"
					(if (s-contains? calibredb-favorite-keyword tag)
					    (format "%s " (propertize calibredb-favorite-icon
								      'face 'calibredb-favorite-face
								      'mouse-face 'calibredb-mouse-face
								      'help-echo "Filter the favorite items"
								      'keymap favorite-map)) "")
					(cond
					 ((s-contains? calibredb-archive-keyword tag)
					  (propertize title 'face 'calibredb-archive-face))
					 ((s-contains? calibredb-highlight-keyword tag)
					  (propertize title 'face 'calibredb-highlight-face))
					 (t
					  (propertize title 'face (calibredb-title-face))))) (calibredb-title-width) :left)
       (calibredb-format-column (propertize format
					    'face 'calibredb-format-face
					    'mouse-face 'calibredb-mouse-face
					    'help-echo "Filter with this format"
					    'keymap format-map) (calibredb-format-width) :left)
       (calibredb-format-column (propertize (s-left 10 date) 'face 'calibredb-date-face ; only keep YYYY-MM-DD
					    'mouse-face 'calibredb-mouse-face
					    'help-echo "Filter with this date"
					    'keymap date-map) (calibredb-date-width) :left)
       (calibredb-format-column (mapconcat
				 (lambda (author)
				   (propertize author
					       'author author
					       'face 'calibredb-author-face
					       'mouse-face 'calibredb-mouse-face
					       'help-echo (format "Filter with this author: %s" author)
					       'keymap author-map))
				 (split-string author "&" t "\s+") " & ") (calibredb-author-width) :left)
       (calibredb-format-column (mapconcat
				 (lambda (tag)
				   (propertize tag
					       'tag tag
					       'face 'calibredb-tag-face
					       'mouse-face 'calibredb-mouse-face
					       'help-echo (format "Filter with this tag: %s" tag)
					       'keymap tag-map))
				 (split-string tag ",") ",") (calibredb-tag-width) :left)
       (calibredb-format-column (propertize ids 'face 'calibredb-ids-face) (calibredb-ids-width) :left)
       (if (stringp comment)
	   (propertize
	    (let ((c (if calibredb-condense-comments (calibredb-condense-comments comment) comment))
		  (w calibredb-comment-width))
	      (cond ((> w 0) (s-truncate w c))
		    ((= w 0) "")
		    (t c)))
	    'face 'calibredb-comment-face) "")
       (format "%s%s"
	       (if calibredb-size-show
		   (propertize size 'face 'calibredb-size-face) "")
	       (if calibredb-size-show
		   (propertize "Mb" 'face 'calibredb-size-face) ""))) )))

;; ** citar

(defun my/citar-toggle-multiple ()
  (interactive)
  (if citar-select-multiple
      (setq citar-select-multiple nil)
    (setq citar-select-multiple t)))

(use-package citar
  :hook (org-mode . citar-capf-setup)
  :custom
  ;; NOTE: Having large bibtex files slows down org-mode through bibtex
  ;;(org-cite-global-bibliography '("~/Zotero/bibtex-export.bib" "~/cat.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-csl-styles-dir
   (expand-file-name "~/Zotero/styles/"))
  (citar-bibliography org-cite-global-bibliography)
  :config
  (setq citar-at-point-function 'embark-act)
  (setq citar-select-multiple nil)
  :bind
  (:map org-mode-map :package org
	("C-c b" . #'org-cite-insert)
	("C-c B" . citar-dwim))
  (:map citar-map :package citar
	("x" . my/citar-toggle-multiple)
	("R" . citar-insert-reference))
  :bind-keymap
  ("C-c c" . citar-map))

(use-package citar-embark
  :disabled
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-denote
  :ensure t
  :custom
  ;; Use package defaults
  (citar-open-always-create-notes nil)
  (citar-denote-file-type 'org)
  (citar-denote-subdir t)
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "author-year-title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :init
  (citar-denote-mode)
  ;; Bind all available commands
  :bind
  (:map citar-map
	("C-n" . citar-create-note)
	("N" . citar-denote-open-note)
	("d" . citar-denote-dwim)
	("C-e" . citar-denote-open-reference-entry)
	("s" . citar-denote-find-reference)
	("c" . citar-denote-find-citation)
	("R" . citar-denote-link-reference)))

;; ** ebib

(use-package ebib
  :config
  (setq ebib-preload-bib-files '("~/Zotero/bibtex-export.bib")))

;; ** speed-type

(use-package speed-type)

;; ** fireplace

(use-package fireplace)

;; ** gptel

(use-package gptel
  :bind
  ("C-c S" . gptel)
  ("C-c s" . gptel-menu)
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-directives '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
		      (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
		      (writing . "You are a large language model and a writing assistant. Respond concisely.")
		      (chat . "You are a large language model and a conversation partner. Respond concisely."))))

(use-package gptel-extensions
  :after gptel
  :straight (gptel-extensions :host github :repo "kamushadenes/gptel-extensions.el" :files ("*.el")))

;; ** anki-helper

(defun my/show-anki ()
  (interactive)
  (shell-command "open -a Anki"))

(use-package anki-helper
  :straight (anki-helper :type git :host github :repo "Elilif/emacs-anki-helper")
  :bind (:map org-mode-map
	      ("C-c r a" . anki-helper-entry-sync)
	      ("C-c r A" . anki-helper-entry-sync-all)
	      ("C-c r d" . anki-helper-entry-delete)
	      ("C-c r D" . anki-helper-entry-delete-all)
	      ("C-c r u" . anki-helper-entry-update)
	      ("C-c r U" . anki-helper-entry-update-all)
	      ("C-c r s" . my/show-anki)))

;; ** pdf-view

(defun my/background-pdf-view-refresh (appearance)
  (cl-loop for buf in (buffer-list)
	   collect
	   (with-current-buffer buf
	     (when (eq major-mode 'pdf-view-mode)
	       (my/pdf-view-themed-minor-mode-refresh)))))

(defun my/pdf-view-themed-minor-mode-refresh ()
  (interactive)
  (pdf-view-themed-minor-mode 1))

(defun my/pdf-view-current-page ()
  (interactive)
  (message "%d/%d" (pdf-view-current-page) (pdf-info-number-of-pages)))

(defun my/pdf-view-open-externally ()
  (interactive)
  (shell-command (concat "open '" buffer-file-name "'")))

(use-package pdf-view
  :straight nil
  :after pdf-tools
  :custom
  (pdf-view-resize-factor 1.05)
  (pdf-view-display-size 'fit-page)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :bind
  (:map pdf-view-mode-map
	("C-c C-o" . my/pdf-view-open-externally)
	("C-c C-r r" . my/pdf-view-themed-minor-mode-refresh)
	("c" . my/pdf-view-current-page)
	("o" . pdf-outline)
	("C-c C-n" . org-noter)))

(use-package saveplace-pdf-view
  :config
  (save-place-mode 1))

;; ** pdf-tools

(use-package pdf-tools
  :defer 2
  :hook (pdf-outline-buffer-mode . visual-line-mode)
  :config
  (pdf-tools-install :no-query)
  (use-package pdf-occur :straight nil))

;; ** pdf-annot

(use-package pdf-annot
  :straight nil
  :after pdf-tools
  :bind
  (:map pdf-annot-minor-mode-map
	("a D" . pdf-annot-delete)
	("a a" . pdf-annot-attachment-dired)
	("a h" . pdf-annot-add-highlight-markup-annotation)
	("a l" . pdf-annot-list-annotations)
	("a m" . pdf-annot-add-markup-annotation)
	("a o" . pdf-annot-add-strikeout-markup-annotation)
	("a s" . pdf-annot-add-squiggly-markup-annotation)
	("a t" . pdf-annot-add-text-annotation)
	("a u" . pdf-annot-add-underline-markup-annotation)))

;; ** image

(use-package image
  :straight nil
  :bind
  (:map image-slice-map
	("C-<mouse-4>" . nil)
	("C-<mouse-5>" . nil)
	("C-<wheel-up>" . nil)
	("C-<wheel-down>" . nil)))
;; ** gnuplot

(use-package gnuplot-mode)
(use-package gnuplot)

;; ** ebdb

(use-package ebdb)

;; ** zotra

(use-package zotra
  :disabled
  :config
  (setq zotra-backend 'zotra-server)
  (setq zotra-local-server-directory "~/repos/zotra-server"))

;; * enable all commands

(defun enable-all-commands ()
  "Enable all commands, reporting on which were disabled."
  (interactive)
  (with-output-to-temp-buffer "*Commands that were disabled*"
    (mapatoms
     (function
      (lambda (symbol)
	(when (get symbol 'disabled)
	  (put symbol 'disabled nil)
	  (prin1 symbol)
	  (princ "\n")))))))

;; * center frame
;; from https://christiantietze.de/posts/2022/04/emacs-center-window-current-monitor-simplified/
(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

;; * CUSTOM LISP
;; ** ox-11ty

(require 'ox-11ty)

;; ** custom-org
(require 'custom-org)
;; ** xah

(use-package xah
  :straight nil
  :bind
  (:map lisp-interaction-mode-map
        ("M-a" . xah-backward-left-bracket)
	("M-e" . xah-forward-right-bracket)
	("C-a" . beginning-of-line)
	("C-e" . end-of-line)
	;;("(" . xah-insert-paren)
	;;(")" . xah-insert-paren)
	;;("{" . xah-insert-brace)
	;;("}" . xah-insert-brace)
	;;("[" . xah-insert-bracket)
	;;("]" . xah-insert-bracket)
	;;("\"" . xah-insert-ascii-double-quote)
	("M-<DEL>" . xah-delete-backward-bracket-text))
  (:map emacs-lisp-mode-map
        ("M-a" . xah-backward-left-bracket)
	("M-e" . xah-forward-right-bracket)
	("C-a" . beginning-of-line)
	("C-e" . end-of-line)
	;;("(" . xah-insert-paren)
	;;(")" . xah-insert-paren)
	;;("{" . xah-insert-brace)
	;;("}" . xah-insert-brace)
	;;("[" . xah-insert-bracket)
	;;("]" . xah-insert-bracket)
	;;("\"" . xah-insert-ascii-double-quote)
	("M-<DEL>" . xah-delete-backward-bracket-text))
  (:map lisp-data-mode-map
	("M-a" . xah-backward-left-bracket)
	("M-e" . xah-forward-right-bracket)
	("C-a" . beginning-of-line)
	("C-e" . end-of-line)
	("M-<DEL>" . xah-delete-backward-bracket-text)))

;; ** lorem-ipsum

(use-package lorem-ipsum
  :straight nil
  :commands (Lorem-ipsum-insert-sentences Lorem-ipsum-insert-list Lorem-ipsum-insert-paragraphs))

;; ** svelte-ts-mode

(use-package svelte-ts-mode
  :straight nil
  :commands (svelte-ts-mode)
  :mode "\\.svelte\\'"
  :hook
  (svelte-ts-mode . (lambda () (apheleia-mode -1)))
  (svelte-ts-mode . (lambda () (setq tab-width 2)))
  :custom-face
  (font-lock-bracket-face ((t (:foreground "tan3")))))

;; * LOCAL-VARIABLES

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; eval:(outline-hide-sublevels 2)

;; Local Variables:
;; outline-regexp: " *;; \\*+"
;; page-delimiter: " *;; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-body)
;; coding: utf-8-unix
;; End:
