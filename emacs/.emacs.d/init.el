;; -*- lexical-binding: t; -*-

;; * Basic settings

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

;; * Packages

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

(require 'use-package-ensure)
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
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; * Themes

(setq custom-safe-themes t)

(setq custom-safe-themes t)
(defalias 'my/apply-theme-change 'my/modus-theme-change)

(add-to-list 'ns-system-appearance-change-functions 'my/apply-theme-change)
;; (add-to-list 'after-make-frame-functions '(lambda (_)
;; (my/apply-theme-change ns-system-appearance))) ;; DOES NOT WORK
(push '(lambda (_) (my/apply-theme-change ns-system-appearance)) (cdr (last after-make-frame-functions)))
(use-package ef-themes)
(use-package color-theme-modern)

;; * Modus themes

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
	    (fg-heading-2 blue-faint)
	    (fg-heading-3 magenta-faint)
	    (fg-heading-4 blue-faint)
	    (fg-heading-5 magenta-faint)
	    (fg-heading-6 blue-faint)
	    (fg-heading-7 magenta-faint)
	    (fg-heading-8 blue-faint)
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
	    (bg-tab-other bg-main))))

;; * Timu theme

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

;; * Poet theme

(use-package poet-theme
  :config
  (setq poet-theme-variable-pitch-multiplier 1.6)
  (setq poet-theme-variable-headers nil))

(add-to-list 'default-frame-alist '(font . "Iosevka Comfy-18"))

;; * Writroom

(defun my/writeroom-mode-hook ()
  "Custom behaviours for `writeroom-mode'."
  (if writeroom-mode
      (progn (centered-cursor-mode 1)
             (display-line-numbers-mode 0))
    (centered-cursor-mode 0)))

(use-package writeroom-mode
  :hook (writeroom-mode . my/writeroom-mode-hook))
(use-package centered-cursor-mode)

;; * Spacious padding

(defun my/spacious-padding-reset ()
  "reset the spacious padding and modeline formats"
  (interactive)
  (spacious-padding-mode 1))

(use-package spacious-padding
  :config
  ;; (spacious-padding-mode)
  )

;; * Pulsar

(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1)
  :hook
  (xref-after-return . pulsar-pulse-line)
  (xref-after-jump . pulsar-pulse-line))

(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
(remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

;; * default-text-scale

(use-package default-text-scale
  :demand t
  :bind
  (:map default-text-scale-mode-map
        ("s-+" . default-text-scale-increase)
        ("s-_" . default-text-scale-decrease))
  :config
  (default-text-scale-mode))

;; * rainbow-delimiters

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

;; * user details

(setq user-full-name "Marc Wenzlawski"
      user-mail-address "marcwenzlawski@gmail.com")

;; * use-package emacs

(use-package emacs
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
  (setq scroll-conservatively 5)
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
  (show-paren-mode)
  (global-auto-revert-mode)
  (push '(lambda (_) (menu-bar-mode -1)) (cdr (last after-make-frame-functions)))

  :custom-face
  (show-paren-match ((t (:underline nil :inverse-video nil))))
  :bind
  ("C-x C-l" . nil)
  ("C-x C-S-l" . downcase-region)
  ("C-c o" .  occur)
  ("C-x M-k" . kill-this-buffer)
  ("C-x <C-i>" . tab-to-tab-stop)
  (:map tab-prefix-map
        ("h" . tab-bar-mode)
        ("s" . tab-switcher))
  (:map help-map
        ("t" . nil)
        ("W" . woman)
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
;; * window

(use-package window
  :config
  (setq fit-window-to-buffer-horizontally t))
;; * CUSTOM FILE

(use-package cus-edit
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

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp"))

;; * outline

(use-package outline
  :custom
  (outline-minor-mode-prefix ""))

;; * editorconfig

(use-package editorconfig
  :config
  (editorconfig-mode))

;; * dtrt-indent

(use-package dtrt-indent
  :config
  (setq dtrt-indent-verbosity 0))
;; (add-to-list 'dtrt-indent-hook-mapping-list '(lua-ts-mode lua lua-ts-indent-offset))
;; (dtrt-indent-global-mode))

;; * gcmh

(use-package gcmh
  :config
  (gcmh-mode 1))

;; * ace-window

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

;; * tabspaces

(use-package tabspaces)

;; * avy

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

;; * embark

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

;; * vterm

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
	  ("woman" woman)
	  ("tldr" tldr)
          ("ediff-files" ediff-files)))
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell (executable-find "fish")))

;; * tldr

(use-package tldr
  :custom-face
  (tldr-command-itself ((t (:inherit font-lock-keyword-face :weight bold :background unspecified :foreground "orange"))))
  (tldr-command-argument ((t nil)))
  (tldr-code-block ((t (:foreground unspecified :background unspecified)))))

;; * devdocs

(use-package devdocs
  :ensure t
  :bind (:map help-map ("D" . devdocs-lookup)))

;; * hydra

(use-package hydra
  :bind
  (:map outline-minor-mode-map
	("C-c <tab>" . hydra-outline/body))
  :config
  (with-eval-after-load 'outline
    (defhydra hydra-outline (:color pink :hint nil)
      "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

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
      ("z" nil "leave"))))

;; * ns-auto-titlebar

(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode))
  (use-package osx-trash
    :config
    (osx-trash-setup)))

;; * highlight visual line

(defun my/highlight-visual-line ()
  (save-excursion
    (cons (progn (beginning-of-visual-line) (point))
          (progn (end-of-visual-line) (point)))))
(setq hl-line-range-function 'my/highlight-visual-line)

;; * openwith

(use-package openwith)

;; * undo-fu

(use-package undo-fu)

(use-package undo-fu-session)

;; * vundo

(use-package vundo)

;; * hl-todo

(use-package hl-todo)

;; * exiftool

(use-package exiftool
  :defer t)

;; * bookmark+

(use-package bookmark+
  :straight (bookmark+))

(setq bookmark-save-flag 1)

;; * helpful

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

;; * tab-bar

(use-package tab-bar
  :custom
  (tab-bar-select-tab-modifiers '(super))
  :config
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; * scratch

(use-package scratch
  :straight (:host codeberg :repo "emacs-weirdware/scratch" :files ("*.el")))

;; * pandoc-mode

(use-package pandoc-mode
  :hook ((text-mode doc-view-mode pdf-view-mode) . pandoc-mode)
  :bind (:map pandoc-mode-map
	      ("C-c p" . pandoc-main-hydra/body)
	      ("C-c /" . nil)))

;; * exec-path-from-shell

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; * transpose-frame

(use-package transpose-frame
  :straight (:host github :repo "emacsorphanage/transpose-frame")
  :bind
  ("C-x 4 t" . transpose-frame)
  ("C-x 4 i" . flip-frame)
  ("C-x 4 o" . flop-frame)
  ("C-x 4 n" . rotate-frame))

;; * modeline

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

;; * hide-mode-line

(use-package hide-mode-line
  :bind (:map help-map ("t m" . hide-mode-line-mode)))

;; * corfu

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

;; * dabbrev

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; * cape

(use-package cape)

;; * expand-region

(use-package expand-region :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))
;; * completion

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

;; * marginalia

(use-package marginalia
  :init
  (marginalia-mode))

;; * orderless

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; * consult

(use-package consult
  :after org
  :config
  (consult-customize consult-notes my/consult-notes-other-window :preview-key "M-.")
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
        ("C-c h" . consult-org-heading))
  (:map python-ts-mode-map
	("M-o" . consult-imenu)))

;; ** consult-flycheck

(use-package consult-flycheck)

;; ** consult-frecoll

(use-package consult-recoll
  :after citar
  :config
  ;; (setq exec-path (append exec-path '("/usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS/")))
  (consult-recoll-embark-setup))

;; ** consult-notes

(use-package consult-notes
  :after consult denote
  :custom-face
  (consult-notes-sep ((t (:foreground "CornFlowerBlue"))))
  :bind
  ("C-c n o" . consult-notes)
  ("C-c n X" . consult-notes-search-in-all-notes)
  ("C-c n 4 o" . my/consult-notes-other-window)
  :config
  (setq consult-notes-file-dir-sources
        '(("Org"             ?o "~/Dropbox/Org/")))
  (setq consult-notes-org-headings-files '("~/Dropbox/Org/"))
  ;;(consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (setq consult-notes-denote-display-id nil)
    (consult-notes-denote-mode))

  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files)))

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

;; ** consult-flyspell

(use-package consult-flyspell
  :bind (:map flyspell-mode-map ("C-<" . consult-flyspell))
  :config
  ;; default settings
  (setq consult-flyspell-select-function 'flyspell-correct-at-point
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

;; * vertico

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
  :after vertico
  :init
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-buffer flat))))

;; * savehist

(use-package savehist
  :init
  (savehist-mode))

;; * eglot

(use-package eglot
  :bind
  (:map eglot-mode-map
	("C-c e f" . eglot-format)
        ("C-c e q" . eglot-shutdown)
        ("C-c e Q" . eglot-shutdown-all)
        ("C-c e l" . eglot-list-connections)
	("C-c e r" . eglot-rename))
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit highlight)))))

;; * flymake

(use-package flymake
  :pin gnu-elpa
  :bind
  (:map flymake-mode-map
	("C-c M-n" . flymake-goto-next-error)
	("C-c M-p" . flymake-goto-prev-error)
	("C-c M-l" . flymake-show-project-diagnostics)))

;; * lisp

(use-package lisp-mode
  :hook (lisp-data-mode . electric-pair-mode))
;; * elisp

(use-package elisp-mode
  :hook (emacs-lisp-mode . electric-pair-mode)
  :bind
  (:map emacs-lisp-mode-map
	("M-i" . completion-at-point))
  (:map lisp-interaction-mode-map
	("M-i" . completion-at-point)))

;; * python

(use-package python
  :bind
  (:map python-ts-mode-map
	("C-c M-e" . eglot)
	("M-i" . completion-at-point))
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

;; * ein

(use-package ein
  :ensure t
  :config)

(use-package ob-ein
  :after ein python-ts-mode
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((ein . t))))

;; * ess

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

;; * julia

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

;; * markdown

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))
;; * typst

(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :custom
  (typst-ts-mode-watch-options "--open"))

;; * fish

(use-package fish-mode
  :config
  (setq fish-enable-auto-indent t))
;; * flycheck flyspell

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

;; * tree-sitter

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
  :config
  (global-treesit-auto-mode))

;; * apheleia

(use-package apheleia
  :bind
  (:map prog-mode-map ("C-c f" . apheleia-format-buffer))
  :config

  (with-eval-after-load 'julia-mode
    (push
     '(julia . ((dir-concat user-emacs-directory "scripts/julia-format.sh") inplace ))
     apheleia-formatters)
    (add-to-list 'apheleia-mode-alist '(julia-mode . julia)))

  (apheleia-global-mode))

;; * format-all

;; problem with emacs format region
;; but apheleia does not have format region.
(use-package format-all
  :disabled)

;; * eldoc

(use-package eldoc
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  :config
  (setq eldoc-current-idle-delay 0.3))

;; * pos-tip

(use-package pos-tip)

;; * yasnippet

(use-package yasnippet
  :disabled 
  :init
  (use-package yasnippet-snippets)
  ;; (setq yas-minor-mode-map
  ;;       (let ((map (make-sparse-keymap)))
  ;;         (define-key map (kbd "s") 'yas-insert-snippet)
  ;;         (define-key map (kbd "n") 'yas-new-snippet)
  ;;         (define-key map (kbd "v") 'yas-visit-snippet-file)
  ;;         map))
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-verbosity 0))

(use-package yasnippet-capf
  :disabled
  :straight (:host github :repo "elken/yasnippet-capf")
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))
;; :bind-keymap ("C-c s" . yas-minor-mode-map))

;; * tempel

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
  :hook
  ((conf-mode prog-mode text-mode) . tempel-setup-capf))

(use-package tempel-collection
  :after tempel)

;; * projectile

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

;; * quickrun

(use-package quickrun)

;; * copilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook prog-mode
  :commands copilot-login
  :bind (:map copilot-completion-map ("<C-i>" . copilot-accept-completion))
  :config
  (setq copilot-idle-delay 0.3))

(use-package jsonrpc
  :pin gnu-elpa)

;; * lua

(use-package lua-mode
  :config
  (setq lua-indent-level 3))
(use-package lua-ts-mode
  :hook (lua-ts-mode . (lambda nil (setq tab-width 3)))
  :config
  (setq lua-ts-indent-offset 3)
  :straight (:host sourcehut :repo "johnmuhl/lua-ts-mode" :files ("*.el")))

;; * cc-mode

(use-package cc-mode
  :hook (awk-mode . (lambda nil (setq tab-width 4))))

;; * emmet

(use-package emmet-mode
  :commands (emmet-find-left-bound emmet-transform emmet-reposition-cursor)
  :hook (html-mode . emmet-mode))

;; * grep

(use-package grep
  :config
  (when (executable-find "rg")
    (setq grep-command "rg --no-heading --line-number --color never %s %s")
    (setq grep-program "rg")))

;; * css

(use-package css-mode
  :hook ((css-mode css-ts-mode) . (lambda nil (setq tab-width 2)))
  :config
  (setq css-indent-offset 2))

;; * rg

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

;; * wgrep

(use-package wgrep)

;; * substitute

(use-package substitute)

;; * occur-x

(use-package occur-x
  :hook (occur-mode . turn-on-occur-x-mode))

;; * loccur

(use-package loccur
  :straight (:host codeberg :repo "fourier/loccur")
  :bind
  (:map isearch-mode-map
        ("M-s l" . loccus-isearch)))

;; * dired

(use-package dired)
(use-package dired-x
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


;; * magit / git

(use-package magit)

(use-package diff-hl)

(use-package git-gutter)

;; * nerd-icons

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; * org

(defun my/org-setup-hook ()
  "Setup org mode hook"
  (display-line-numbers-mode 0)
  ;;(smartparens-mode 0)
  ;;(git-gutter-mode 0)
  (auto-fill-mode 1)
  (setq fill-column 78))

(defun my/org-open-at-point-other-window ()
  "Open at point other window"
  (interactive)
  (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup)))
    (org-open-at-point)))

(defun my/org-open-at-point-other-frame ()
  "Open at point other frame"
  (interactive)
  (let ((org-link-frame-setup (append '((file . find-file-other-frame)) org-link-frame-setup)))
    (org-open-at-point)))


(use-package org
  :pin manual
  ;; :custom
  ;; (display-buffer-alist
  ;;  (append display-buffer-alist
  ;;       '(("^\\(CAPTURE-.+\\)$\\|\\*\\(?:Capture\\|Org Select\\)\\*"
  ;;          (display-buffer-below-selected display-buffer-at-bottom)
  ;;          (inhibit-same-window . t)
  ;;          (window-height . )))))
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-directory "~/Dropbox/Org/")
  (setq org-agenda-files '("daily.org" "refile.org" "future.org"))
  ;;(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING") "DONE"))
  (setq org-hide-emphasis-markers t)
  (setq org-latex-compiler "xelatex")
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 2)))
  (setq org-ellipsis "↴")

  ;; after https://emacs.stackexchange.com/questions/75822/ignoring-non-existent-org-mode-agenda-files
  (setq org-agenda-skip-unavailable-files t)
  (setq org-src-preserve-indentation t)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-fast-tag-selection-single-key t)
  (setq org-special-ctrl-a/e t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-max-level 5)
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (add-to-list 'org-babel-load-languages '(shell . t))
  (setq org-src-window-setup 'current-window)
  (setq org-capture-templates
        '(("r" "refile" entry (file "~/Dropbox/Org/refile.org") "* %^{Title} %^g\n%U\n\n%?" :prepend t :empty-lines-after 1)
          ("t" "today" entry (file+olp+datetree "~/Dropbox/Org/daily.org") "* %^{Title}\n\n%?")
          ("j" "Journal" entry (file+olp+datetree "~/Dropbox/Org/journal.org") "* %U %^{Title}\n%i\n\n%?")))
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . visual-line-mode)
  :bind
  ("C-x c" . org-capture)
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("C-c a" . org-agenda)
        ("C-c 4 C-o" . my/org-open-at-point-other-window)
        ("C-c 4 o" . my/org-open-at-point-other-window)
        ("C-c 5 C-o" . my/org-open-at-point-other-frame)
        ("C-c 5 o" . my/org-open-at-point-other-frame)
        ("C-c e" . org-emphasize)
	("<C-i>" . org-delete-backward-char)
	("M-i" . backward-kill-word))
  :custom-face
  (org-document-title ((t (:height 1.7)))))

;; (use-package ob-shell
;;   :after org
;;   :config
;;   (setq org-babel-default-header-args:sh '((:results . "output")))
;;   (setq org-babel-default-header-args:shell '((:results . "output"))))

;; * org-appear

(use-package org-appear
  :hook org-mode)

;; * ox-hugo

(use-package ox-hugo
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

;; * ox-pandoc

(use-package ox-pandoc
  :after ox
  :custom
  (org-pandoc-menu-entry
   '(
     ;;(?0 "to jats." org-pandoc-export-to-jats)
     ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
     ;;(?  "as jats." org-pandoc-export-as-jats)
     ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
     ;;(?! "to epub2." org-pandoc-export-to-epub2)
     ;;(?2 "to tei." org-pandoc-export-to-tei)
     ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
     ;;(?" "as tei." org-pandoc-export-as-tei)
     ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
     ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
     ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
     ;;(?4 "to html5." org-pandoc-export-to-html5)
     (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
     (?$ "as html5." org-pandoc-export-as-html5)
     (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
     (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
     ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
     ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
     ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
     ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
     ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
     ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
     ;;(?8 "to opendocument." org-pandoc-export-to-opendocument)
     ;;(?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
     ;;(?( "as opendocument." org-pandoc-export-as-opendocument)
     ;;(?9 "to opml." org-pandoc-export-to-opml)
     ;;(?9 "to opml and open." org-pandoc-export-to-opml-and-open)
     ;;(?) "as opml." org-pandoc-export-as-opml)
     ;;(?: "to rst." org-pandoc-export-to-rst)
     ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
     ;;(?* "as rst." org-pandoc-export-as-rst)
     ;;(?< "to slideous." org-pandoc-export-to-slideous)
     ;; (?\[ "to jira." org-pandoc-export-to-jira)
     ;; (?\[ "as jira." org-pandoc-export-as-jira)
     ;; (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
     ;; (?, "as slideous." org-pandoc-export-as-slideous)
     (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
     (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
     ;;(?> "to textile." org-pandoc-export-to-textile)
     ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
     ;;(?. "as textile." org-pandoc-export-as-textile)
     ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
     ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
     ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
     (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
     (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
     ;; (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
     ;; (?C "to context-pdf." org-pandoc-export-to-context-pdf)
     ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
     (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
     (?D "as docbook5." org-pandoc-export-as-docbook5)
     ;; (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
     ;; (?E "to epub3." org-pandoc-export-to-epub3)
     ;;(?f "to fb2." org-pandoc-export-to-fb2)
     ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
     ;;(?F "as fb2." org-pandoc-export-as-fb2)
     ;;(?g "to gfm." org-pandoc-export-to-gfm)
     (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
     (?G "as gfm." org-pandoc-export-as-gfm)
     ;;(?h "to html4." org-pandoc-export-to-html4)
     (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
     (?H "as html4." org-pandoc-export-as-html4)
     ;;(?i "to icml." org-pandoc-export-to-icml)
     ;; (?i "to icml and open." org-pandoc-export-to-icml-and-open)
     ;; (?I "as icml." org-pandoc-export-as-icml)
     ;;(?j "to json." org-pandoc-export-to-json)
     (?j "to json and open." org-pandoc-export-to-json-and-open)
     (?J "as json." org-pandoc-export-as-json)
     ;;(?k "to markdown." org-pandoc-export-to-markdown)
     ;;(?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
     ;;(?K "as markdown." org-pandoc-export-as-markdown)
     (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
     (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
     ;;(?m "to man." org-pandoc-export-to-man)
     (?m "to man and open." org-pandoc-export-to-man-and-open)
     (?M "as man." org-pandoc-export-as-man)
     ;;(?n "to native." org-pandoc-export-to-native)
     (?n "to native and open." org-pandoc-export-to-native-and-open)
     (?N "as native." org-pandoc-export-as-native)
     (?o "to odt and open." org-pandoc-export-to-odt-and-open)
     (?O "to odt." org-pandoc-export-to-odt)
     (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
     (?P "to pptx." org-pandoc-export-to-pptx)
     ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
     ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
     ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
     ;;(?r "to rtf." org-pandoc-export-to-rtf)
     (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
     (?R "as rtf." org-pandoc-export-as-rtf)
     ;;(?s "to s5." org-pandoc-export-to-s5)
     ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
     ;;(?S "as s5." org-pandoc-export-as-s5)
     ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
     ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
     ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
     (?< "to typst." org-pandoc-export-to-typst)
     (?, "to typst and open." org-pandoc-export-to-typst-and-open)
     ;; (?, "as typst." org-pandoc-export-as-typst)
     (?> "to typst-pdf." org-pandoc-export-to-typst-pdf)
     (?. "to typst-pdf and open." org-pandoc-export-to-typst-pdf-and-open)
     ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
     ;; (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
     ;; (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
     ;;(?v "to revealjs." org-pandoc-export-to-revealjs)
     ;; (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
     ;; (?V "as revealjs." org-pandoc-export-as-revealjs)
     ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
     ;; (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
     ;; (?W "as mediawiki." org-pandoc-export-as-mediawiki)
     (?x "to docx and open." org-pandoc-export-to-docx-and-open)
     (?X "to docx." org-pandoc-export-to-docx)
     ;;(?y "to slidy." org-pandoc-export-to-slidy)
     ;; (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
     ;; (?Y "as slidy." org-pandoc-export-as-slidy)
     ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
     ;; (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
     ;; (?Z "as dzslides." org-pandoc-export-as-dzslides)
     ;;(?{ "to muse." org-pandoc-export-to-muse)
     ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
     ;;(?[ "as muse." org-pandoc-export-as-muse)
     ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
     ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
     ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
     ;;(?~ "to haddock." org-pandoc-export-to-haddock)
     ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
     ;;(?^ "as haddock." org-pandoc-export-as-haddock)
     )))


;; * ob-python

(use-package ob-python
  :after org
  :config
  (setq org-babel-python-command "python3"))

;; * ob-ipython

(use-package ob-ipython
  :disabled
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t))))

;; * ob-julia

(use-package ob-julia
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((julia . t))))

;; (use-package ob-julia-vterm
;;   :after org
;;   :config
;;   (add-to-list 'org-babel-load-languages '(julia-vterm . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;;   (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm))

(with-eval-after-load 'ob-julia-vterm
  (defun ob-julia-vterm-make-str-to-run (uuid params src-file out-file)
    "Make Julia code that execute-s the code in SRC-FILE depending on PARAMS.
The results are saved in OUT-FILE.  UUID is a unique id assigned
to the evaluation."
    (format
     (pcase (cdr (assq :result-type params))
       ('output "\
#OB-JULIA-VTERM_BEGIN %s
import Logging; let
    out_file = \"%s\"
    open(out_file, \"w\") do io
        logger = Logging.ConsoleLogger(io)
        redirect_stdout(io) do
            try
                include(\"%s\")
                # %s %s
            catch e
                showerror(logger.stream, e, %s)
            end
        end
    end
    result = open(io -> println(read(io, String)), out_file)
    if result == nothing
        open(out_file, \"a\") do io
            print(io, \"\n\")
        end
    else
        result
    end
end #OB-JULIA-VTERM_END\n")
       ('value "\
#OB-JULIA-VTERM_BEGIN %s
import Logging, Pkg; open(\"%s\", \"w\") do io
    logger = Logging.ConsoleLogger(io)
    try
        result = include(\"%s\")
        if !(result isa Nothing)
            if isdefined(Main, :PrettyPrinting) && isdefined(PrettyPrinting, :pprint) ||
               \"PrettyPrinting\" in [p.name for p in values(Pkg.dependencies())]
                @eval import PrettyPrinting
                Base.invokelatest(PrettyPrinting.pprint, io, result)
            else
                Base.invokelatest(print, io, result)
            end
        else
            if %s
                Base.invokelatest(show, io, \"text/plain\", result)
            else
                Base.invokelatest(show, IOContext(io, :limit => true), \"text/plain\", result)
            end
        end
        result
    catch e
        msg = sprint(showerror, e, %s)
        println(logger.stream, msg)
        println(msg)
        nothing
    end
end #OB-JULIA-VTERM_END\n"))
     (substring uuid 0 8) out-file src-file
     (if (member "pp" (cdr (assq :result-params params))) "true" "false")
     (if (member "nolimit" (cdr (assq :result-params params))) "true" "false")
     (if (not (member (cdr (assq :debug params)) '(nil "no"))) "catch_backtrace()" ""))))


;; * ob-shell

(use-package ob-shell
  :after org
  :config
  (setq org-babel-default-header-args:sh '((:results . "output")))
  (setq org-babel-default-header-args:shell '((:results . "output"))))
;; * ob-async

;; python does not work.
(use-package ob-async
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "ipython"))
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
		(setq inferior-julia-program-name "/usr/local/bin/julia"))))
;; * htmlize

(use-package htmlize)

;; * org-remark

(use-package org-remark
  :bind (;; :bind keyword also implicitly defers org-remark itself.
	 ;; Keybindings before :map is set for global-map.
	 :map org-remark-mode-map
	 ("C-c r m" . org-remark-mark)
	 ("C-c r l" . org-remark-mark-line)
	 ("C-c r o" . org-remark-open)
	 ("C-c r n" . org-remark-next)
	 ("C-c r p" . org-remark-prev)
	 ("C-c r ]" . org-remark-view-next)
	 ("C-c r [" . org-remark-view-prev)
	 ("C-c r r" . org-remark-remove)
	 ("C-c r d" . org-remark-delete)
	 ("C-c r v" . org-remark-view))
  :init
  ;; (org-remark-global-tracking-mode +1)
  :hook (org-remark-open . (lambda () (org-cycle-hide-drawers 'all)))
  :config
  (setq org-remark-notes-file-name "~/Dropbox/Org/remark.org"
	org-remark-line-minimum-left-margin-width 1
	org-remark-line-heading-title-max-length 70))
;;(use-package org-remark-nov  :after nov  :config (org-remark-nov-mode +1)))

;; * org-mac-link

(when (eq system-type 'darwin)
  (defun my/org-mac-link-applescript-librewolf-get-frontmost-url ()
    "AppleScript to get the links to the frontmost window of the LibreWolf.app."
    (let ((result
	   (org-mac-link-do-applescript
	    (concat
	     "tell application \"System Events\"\n"
	     "   tell its application process \"LibreWolf\"\n"
	     "       set theTitle to get name of window 1\n"
	     "       set theUrl to get value of UI element 1 of combo box 1 of toolbar \"Navigation\" of first group of front window\n"
	     "    end tell\n"
	     "end tell\n"
	     "set theResult to (get theUrl) & \"::split::\" & (get theTitle)\n"
	     "set links to {}\n"
	     "copy theResult to the end of links\n"
	     "return links as string\n"))))
      (car (split-string result "[\r\n]+" t))))

  (defun my/org-mac-link-librewolf-get-frontmost-url ()
    "Get the link to the frontmost window of the LibreWolf.app."
    (interactive)
    (message "Applescript: Getting Firefox url...")
    (org-mac-link-paste-applescript-links (my/org-mac-link-applescript-librewolf-get-frontmost-url)))

  (defun my/org-mac-link-librewolf-insert-frontmost-url ()
    "Insert the link to the frontmost window of the LibreWolf.app."
    (interactive)
    (insert (my/org-mac-link-librewolf-get-frontmost-url)))

  (defun my/org-mac-link-get-link (&optional beg end)
    "Prompt for an application to grab a link from.
  When done, go grab the link, and insert it at point. If a region
  is active, that will be the link's description."
    (interactive
     (if (use-region-p)
	 (list (region-beginning) (region-end))
       '()))
    (let* ((descriptors
	    `(("F" "inder" org-mac-link-finder-insert-selected ,org-mac-link-finder-app-p)
	      ("m" "ail" org-mac-link-mail-insert-selected ,org-mac-link-mail-app-p)
	      ("d" "EVONthink Pro Office" org-mac-link-devonthink-item-insert-selected
	       ,org-mac-link-devonthink-app-p)
	      ("o" "utlook" org-mac-link-outlook-message-insert-selected ,org-mac-link-outlook-app-p)
	      ("a" "ddressbook" org-mac-link-addressbook-item-insert-selected ,org-mac-link-addressbook-app-p)
	      ("s" "afari" org-mac-link-safari-insert-frontmost-url ,org-mac-link-safari-app-p)
	      ("l" "ibrewolf" my/org-mac-link-librewolf-insert-frontmost-url ,org-mac-link-librewolf-app-p)
	      ("v" "imperator" org-mac-link-vimperator-insert-frontmost-url ,org-mac-link-firefox-vimperator-p)
	      ("c" "hrome" org-mac-link-chrome-insert-frontmost-url ,org-mac-link-chrome-app-p)
	      ("b" "rave" org-mac-link-brave-insert-frontmost-url ,org-mac-link-brave-app-p)
	      ("e" "evernote" org-mac-link-evernote-note-insert-selected ,org-mac-link-evernote-app-p)
	      ("t" "ogether" org-mac-link-together-insert-selected ,org-mac-link-together-app-p)
	      ("S" "kim" org-mac-link-skim-insert-page ,org-mac-link-skim-app-p)
	      ("A" "crobat" org-mac-link-acrobat-insert-page ,org-mac-link-acrobat-app-p)
	      ("q" "utebrowser" org-mac-link-qutebrowser-insert-frontmost-url ,org-mac-link-qutebrowser-app-p)))
	   (menu-string (make-string 0 ?x))
	   input)

      ;; Create the menu string for the keymap
      (mapc (lambda (descriptor)
	      (when (elt descriptor 3)
		(setf menu-string (concat menu-string
					  "[" (elt descriptor 0) "]"
					  (elt descriptor 1) " "))))
	    descriptors)
      (setf (elt menu-string (- (length menu-string) 1)) ?:)

      ;; Prompt the user, and grab the link
      (message menu-string)
      (setq input (read-char-exclusive))
      (mapc (lambda (descriptor)
	      (let ((key (elt (elt descriptor 0) 0))
		    (active (elt descriptor 3))
		    (grab-function (elt descriptor 2)))
		(when (and active (eq input key))
		  (if (and beg end)
		      (let ((new-desc (buffer-substring beg end))
			    end-desc)
			(delete-region beg end)
			(call-interactively grab-function)
			(save-excursion
			  (backward-char 2)
			  (setq end-desc (point))
			  (search-backward "][")
			  (forward-char 2)
			  (delete-region (point) end-desc)
			  (insert new-desc)))
		    (call-interactively grab-function)))))
	    descriptors)))

  (use-package org-mac-link
    :demand t
    :init
    (setq org-mac-link-brave-app-p nil
	  org-mac-link-chrome-app-p nil
	  org-mac-link-acrobat-app-p nil
	  org-mac-link-outlook-app-p nil
	  org-mac-link-addressbook-app-p nil
	  org-mac-link-qutebrowser-app-p nil
	  org-mac-link-finder-app-p t
	  org-mac-link-mail-app-p t
	  org-mac-link-devonthink-app-p t
	  org-mac-link-safari-app-p nil
	  org-mac-link-librewolf-app-p t
	  org-mac-link-firefox-vimperator-p nil
	  org-mac-link-evernote-app-p nil
	  org-mac-link-together-app-p nil
	  org-mac-link-skim-app-p t)
    :bind
    (:map org-mode-map
	  ("C-c L" . my/org-mac-link-get-link))))

;; * org-noter

(use-package org-noter
  :bind
  (:map org-noter-doc-mode-map ("q" . nil))
  (:map pdf-view-mode-map ("C-c C-n" . org-noter))
  (:map org-mode-map
	("C-c C-x n n" . org-noter)
	("C-c C-x n k" . org-noter-kill-session)
	("C-c C-x n s" . org-noter-create-skeleton))
  :config
  (add-to-list 'org-noter-notes-search-path "/Users/my/Library/CloudStorage/Dropbox/Org")
  (setq org-noter-default-notes-file-names '("noter.org")
	org-noter-always-create-frame nil
	org-noter-auto-save-last-location t
	org-noter-doc-split-fraction '(0.5 . 0.5)
	org-noter-kill-frame-at-session-end nil
	org-noter-separate-notes-from-heading t))

;; * org-ql

(use-package org-ql)

;; * org-web-tools

(use-package org-web-tools)

;; * gnuplot

(use-package gnuplot-mode)
(use-package gnuplot)
;; * notmuch

(use-package notmuch)

;; * elfeed

(use-package elfeed)
(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/feeds.org"))
  (setq elfeed-search-title-max-width 100)
  (elfeed-org))

;; * eww

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
  (setq eww-header-line-format nil))

(use-package shr
  :bind
  (:map shr-map
	("u" . nil)))

;; * shr

(use-package shr
  :config
  (setq shr-max-image-proportion 0.4))

;; * which-key

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h nil)
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

;; * denote

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
  (require 'denote-org-dblock)
  :hook (dired-mode-hook . denote-dired-mode-in-directories)
  :bind
  ("C-c n n" . denote)
  ("C-c n p" . denote-region) ; "contents" mnemonic
  ("C-c n N" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
  ("C-c n s" . denote-subdirectory)
  ("C-c n t" . denote-template)
  ("C-c n i" . denote-link) ; "insert" mnemonic
  ("C-c n I" . denote-add-links)
  ("C-c n L" . denote-link-or-create)
  ("C-c n l" . denote-link-after-creating)
  ("C-c n h" . denote-org-extras-link-to-heading)
  ("C-c n b" . denote-backlinks)
  ("C-c n f f" . denote-find-link)
  ("C-c n f b" . denote-find-backlink)
  ("C-c n f r" . my/denote-rg-search)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter)
  ("C-c n C-r" . my/denote-rename-buffer))

(defun my/denote-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep denote-directory)))

;; * nov-mode

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

;; * esxml

(use-package esxml)

;; * calibredb

(defun my/refresh-calibre-bib ()
  (interactive)
  (shell-command "calibredb catalog /tmp/cat.bib --fields=title,authors,formats,id,isbn,pubdate,tags,uuid,identifiers" )
  (shell-command "awk -f ~/.emacs.d/scripts/escape_comma.awk /tmp/cat.bib > ~/cat.bib"))

(use-package calibredb
  :bind
  ("C-c d" . calibredb)
  ("C-c C-d" . my/refresh-calibre-bib)
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

;; * citar

(defun my/citar-toggle-multiple ()
  (interactive)
  (if citar-select-multiple
      (setq citar-select-multiple nil)
    (setq citar-select-multiple t)))

(use-package citar
  :hook (org-mode . citar-capf-setup)
  :custom
  (org-cite-global-bibliography '("~/Zotero/bibtex-export.bib" "~/cat.bib"))
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
	("a" . consult-recoll))
  :bind-keymap
  ("C-c c" . citar-map))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-denote
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
	("c" . citar-create-note)
	("N" . citar-denote-open-note)
	("d" . citar-denote-dwim)
	("E" . citar-denote-open-reference-entry)
	("s" . citar-denote-find-reference)
	("S" . citar-denote-find-citation)
	("i" . citar-denote-link-reference)))

;; * ebib

(use-package ebib
  :config
  (setq ebib-preload-bib-files '("~/Zotero/bibtex-export.bib")))

;; * speed-type

(use-package speed-type)

;; * fireplace

(use-package fireplace)

;; * gptel

(use-package gptel)

;; * pdf-view

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

;; * pdf-tools

(use-package pdf-tools
  :defer 2
  :hook (pdf-outline-buffer-mode . visual-line-mode)
  :config
  (pdf-tools-install :no-query)
  (use-package pdf-occur))

;; * pdf-annot

(use-package pdf-annot
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

;; * other

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

(defun my/launch-note (&optional initial-input key)
  (select-frame-set-input-focus (selected-frame))
  (set-frame-size (selected-frame) 80 15)
  (set-frame-name "org-capture")
  (add-hook 'org-capture-after-finalize-hook 'my/post-org-launch-note)
  (letf! ((#'pop-to-buffer #'switch-to-buffer))
	 (interactive)
	 (switch-to-buffer (doom-fallback-buffer))
	 (let ((org-capture-initial initial-input)
	       org-capture-entry)
	   (when (and key (not (string-empty-p key)))
	     (setq org-capture-entry (org-capture-select-template key)))
	   (funcall #'org-capture))
	 )
  )

(defun my/remove-launch-note-hook ()
  (interactive)
  (remove-hook 'org-capture-after-finalize-hook 'my/post-org-launch-note))

(defun my/post-org-launch-note ()
  (my/remove-launch-note-hook)
  (delete-frame))

;; * ox-11ty

(require 'ox-11ty)

;; * custom-org
(require 'custom-org)
;; * xah

(use-package xah
  :bind
  (:map lisp-interaction-mode-map
        ("C-a" . xah-backward-left-bracket)
	("C-e" . xah-forward-right-bracket)
	("M-a" . beginning-of-line)
	("M-e" . end-of-line)
	;;("(" . xah-insert-paren)
	;;(")" . xah-insert-paren)
	;;("{" . xah-insert-brace)
	;;("}" . xah-insert-brace)
	;;("[" . xah-insert-bracket)
	;;("]" . xah-insert-bracket)
	;;("\"" . xah-insert-ascii-double-quote)
	("M-<DEL>" . xah-delete-backward-bracket-text))
  (:map emacs-lisp-mode-map
        ("C-a" . xah-backward-left-bracket)
	("C-e" . xah-forward-right-bracket)
	("M-a" . beginning-of-line)
	("M-e" . end-of-line)
	;;("(" . xah-insert-paren)
	;;(")" . xah-insert-paren)
	;;("{" . xah-insert-brace)
	;;("}" . xah-insert-brace)
	;;("[" . xah-insert-bracket)
	;;("]" . xah-insert-bracket)
	;;("\"" . xah-insert-ascii-double-quote)
	("M-<DEL>" . xah-delete-backward-bracket-text))
  (:map lisp-data-mode-map
	("C-a" . xah-backward-left-bracket)
	("C-e" . xah-forward-right-bracket)
	("M-a" . beginning-of-line)
	("M-e" . end-of-line)
	("M-<DEL>" . xah-delete-backward-bracket-text)))

;; * LOCAL-VARIABLES

;; This is not a literate config tangled from an Org-mode document! So I include
;; some file-specific settings to make it easier to parse. Specifically, the
;; outline that you see in this document is represented in the Lisp files as
;; Org-style collapsible outline headings. See [[*OUTLINE MODE][Outline Mode]].

;; eval:(outline-hide-sublevels 5)

;; Local Variables:
;; outline-regexp: ";; \\*+"
;; page-delimiter: ";; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-body)
;; End:
