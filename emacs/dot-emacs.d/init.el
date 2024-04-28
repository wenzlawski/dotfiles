;;; package --- Summary  -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my personal Emacs configuration file.

;;; Code:

;; * BASIC SETTINGS

(setopt warning-minimum-level :emergency)
(defun dir-concat (dir file)
  "Join path DIR with filename FILE correctly."
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
  (setopt native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setopt native-compile-prune-cache t)) ; Emacs 29


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
(setopt ffap-machine-p-known 'reject)

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

(use-package package
  :hook (package-menu-mode-hook . hl-line-mode)
  :custom
  (package-archives
   '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
     ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
  ;; (setopt package-vc-register-as-project nil) ; Emacs 30
  ;; Highest number gets priority (what is not mentioned has priority 0)
  (package-archive-priorities
   '(("gnu-elpa" . 3)
     ("melpa" . 2)
     ("nongnu" . 1)))
  (package-install-upgrade-built-in nil))

;;(require 'use-package-ensure)
;; (setq use-package-always-ensure nil)

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
;; (straight-use-package '(org :type built-in))
(straight-use-package '(org))

(require 'utils)

;; * THEMES

(setq custom-safe-themes t)

(add-to-list 'ns-system-appearance-change-functions 'my/modus-theme-change)
;; (add-to-list 'after-make-frame-functions '(lambda (_)
;; (my/apply-theme-change ns-system-appearance))) ;; DOES NOT WORK
(push '(lambda (_) (my/modus-theme-initialize ns-system-appearance)) (cdr (last after-make-frame-functions)))
(use-package ef-themes
  :straight t)
(use-package color-theme-modern
  :straight t)

;; ** Modus themes

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  (modus-themes-org-blocks nil)
  (modus-themes-completions '((t . (extrabold))))
  (modus-themes-prompts nil)
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 2.2))
     (agenda-date . (variable-pitch regular 1.3))
     (t . (variable-pitch medium))))
  (modus-themes-custom-auto-reload t)
  :config
  (setopt modus-vivendi-palette-overrides
	  '((bg-main "#070707") ; 1A1A1A
	    (bg-dim "#1A1A1A") ; 0E0E0E
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
	    (fg-line-number-inactive "gray50")
            (fg-line-number-active fg-main)
            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecified)
	    ;; (fg-heading-1 black)
	    ;; (fg-heading-2 olive)
	    ;; (fg-heading-3 slate)
	    ;; (fg-heading-4 maroon)
	    ;; (fg-heading-5 olive)
	    ;; (fg-heading-6 slate)
	    ;; (fg-heading-7 maroon)
	    ;; (fg-heading-8 olive)
	    (identifier magenta-faint)
	    (keybind magenta-cooler)
	    (accent-0 magenta-cooler)
	    (accent-1 cyan-cooler)
	    (accent-2 blue-warmer)
	    (accent-3 red-cooler)
	    (bg-completion bg-blue-nuanced)
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

(defun my/modus-theme-initialize (appearance)
  "Initialize the modus theme."
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark  (load-theme 'modus-vivendi t)))
  (my/modus-theme-on-toggle))

(defun my/modus-theme-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (pcase appearance
    ('light (modus-themes-select 'modus-operandi))
    ('dark  (modus-themes-select 'modus-vivendi))))

(defun my/modus-theme-on-toggle ()
  "Set some faces each toggle."
  ;; (set-face-attribute 'yas-field-highlight-face nil
  ;; 		      :inherit 'region :background (modus-themes-get-color-value 'bg-blue-subtle))
  ;; (set-face-attribute 'eglot-highlight-symbol-face nil
  ;; 		      :bold t :underline nil :background (modus-themes-get-color-value 'bg-yellow-intense))
  )

(add-hook 'modus-themes-after-load-theme-hook #'my/modus-theme-on-toggle)

(defun my/modus-themes-invisible-dividers (&rest _)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg :foreground ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'my/modus-themes-invisible-dividers)

;; ** Timu theme

(use-package timu-macos-theme
  :straight (:host github :repo "emacsmirror/timu-macos-theme")
  :init
  (setq timu-macos-flavour (symbol-name ns-system-appearance)))

(defun my/timu-theme-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (interactive)
  (customize-set-variable 'timu-macos-flavour (symbol-name appearance))
  (load-theme 'timu-macos t))

;; ** Poet theme

(use-package poet-theme
  :straight t
  :custom
  (poet-theme-variable-pitch-multiplier 1.6)
  (poet-theme-variable-headers nil))

;; * USER INTERFACE
;; ** Writeroom

(use-package writeroom-mode
  :disabled
  :straight t
  :hook (writeroom-mode . my/writeroom-mode-hook)
  :custom
  (writeroom-width 100)
  :config
  (defun my/writeroom-mode-hook ()
    "Custom behaviours for `writeroom-mode'."
    (if writeroom-mode
	(progn (centered-cursor-mode 1)
               (display-line-numbers-mode 0))
      (centered-cursor-mode 0))))

(with-eval-after-load 'olivetti
  ;; Distraction-free writing
  (defun my/distraction-free ()
    "Distraction-free writing environment using Olivetti package."
    (interactive)
    (if (equal (bound-and-true-p olivetti-mode) nil)
	(progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-set 2)
          (olivetti-mode t))
      (progn
	(if (eq (length (window-list)) 1)
            (jump-to-register 1))
	(olivetti-mode 0)
	(text-scale-set 0))))

  (defun my/olivetti-org-indent ()
    "Set `olivetti-body-width' according to `org-indent-mode'."
    (if (and (eq major-mode 'org-mode) (bound-and-true-p org-indent-mode))
	(setopt olivetti-body-width
		(+ fill-column
		   (* (-max (org-map-entries 'org-outline-level nil nil))
		      (bound-and-true-p org-indent-indentation-per-level)))))))

(use-package olivetti
  :straight t
  :hook (olivetti-mode-on . my/olivetti-org-indent))

(use-package centered-cursor-mode
  :straight t)

;; ** Pulsar

(use-package pulsar
  :straight t
  :hook
  (xref-after-return . pulsar-pulse-line)
  (xref-after-jump . pulsar-pulse-line)
  :custom-face
  (pulsar-green ((t (:background "#c0e7d4"))))
  :custom
  (window-selection-change-functions '((lambda (_) (if (not (window-minibuffer-p)) (pulsar-pulse-line)))))
  (window-buffer-change-functions '((lambda (_) (if (not (window-minibuffer-p)) (pulsar-pulse-line)))))
  (pulsar-pulse t)
  (pulsar-delay 0.05)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-blue)
  (pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
(remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)

;; ** default-text-scale

(use-package default-text-scale
  :straight t
  :config
  (bind-key "s-+" 'default-text-scale-increase)
  (bind-key "s-_" 'default-text-scale-decrease)
  (default-text-scale-mode))

;; ** rainbow-delimiters

(use-package rainbow-delimiters
  :straight t
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

(use-package ns-auto-titlebar
  :straight t
  :when (eq system-type 'darwin)
  :config
  (ns-auto-titlebar-mode))
(use-package osx-trash
  :straight t
  :when (eq system-type 'darwin)
  :config
  (osx-trash-setup))

;; ** modeline

(require 'prot-modeline)

(defun prot-modeline-subtle-activate ()
  "Run prot-modeline-subtle-mode with 1."
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
;; (setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '((:eval
                 (simple-mode-line-render
                  (quote ("%e"
                          prot-modeline-kbd-macro
                          prot-modeline-narrow
                          prot-modeline-buffer-status
                          prot-modeline-input-method
                          prot-modeline-buffer-identification
                          "  "
                          prot-modeline-major-mode
                          prot-modeline-process
                          "  "
                          prot-modeline-vc-branch
                          "  "
                          prot-modeline-eglot))
		  (quote (
			  " "
			  prot-modeline-misc-info
			  " "))))))
;;(prot-modeline-subtle-mode)

;; ** highlight visual line

(defun my/highlight-visual-line ()
  "Only highlight the visual line."
  (save-excursion
    (cons (progn (beginning-of-visual-line) (point))
	  (progn (end-of-visual-line) (point)))))
(setopt hl-line-range-function 'my/highlight-visual-line)

;; ** hl-todo

(use-package hl-todo
  :straight t
  :config
  (global-hl-todo-mode))

;; ** nerd-icons

(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-scale-factor 1.0)
  )

;; ** fontaine

(setq fontaine-presets
      '(
	(input :default-family "Input Mono")
	(jetbrains :default-family "JetBrains Mono")
	(source-code-pro :default-family "Source Code Pro")
	(dejavu-sans-mono :default-family "DejaVu Sans Mono")
	(fira-code :default-family "Fira Code"
		   :line-spacing 0.05)
	(iosevka :default-family "Iosevka")
	(menlo :default-family "Menlo")
	(unifont :default-family "Unifont")
	(go-mono :default-family "Go Mono")
	(regular)
	(t :default-family "Iosevka"
	   :default-weight regular
	   :default-slant normal
	   :default-height 170
	   :fixed-pitch-family nil
	   :fixed-pitch-weight nil
	   :fixed-pitch-slant nil
	   :fixed-pitch-height 1.0
	   :fixed-pitch-serif-family nil
	   :fixed-pitch-serif-weight nil
	   :fixed-pitch-serif-slant nil
	   :fixed-pitch-serif-height 1.0
	   :variable-pitch-family "iA Writer Quattro V"
	   :variable-pitch-weight nil
	   :variable-pitch-slant nil
	   :variable-pitch-height 1.0
	   :mode-line-active-family nil
	   :mode-line-active-weight nil
	   :mode-line-active-slant nil
	   :mode-line-active-height 1.0
	   :mode-line-inactive-family nil
	   :mode-line-inactive-weight nil
	   :mode-line-inactive-slant nil
	   :mode-line-inactive-height 1.0
	   :header-line-family nil
	   :header-line-weight nil
	   :header-line-slant nil
	   :header-line-height 1.0
	   :line-number-family nil
	   :line-number-weight nil
	   :line-number-slant nil
	   :line-number-height 1.0
	   :tab-bar-family nil
	   :tab-bar-weight nil
	   :tab-bar-slant nil
	   :tab-bar-height 1.0
	   :tab-line-family nil
	   :tab-line-weight nil
	   :tab-line-slant nil
	   :tab-line-height 1.0
	   :bold-family nil
	   :bold-slant nil
	   :bold-weight bold
	   :bold-height 1.0
	   :italic-family nil
	   :italic-weight nil
	   :italic-slant italic
	   :italic-height 1.0
	   :line-spacing 0.05)
	))

(use-package fontaine
  :straight t
  :demand t
  :hook
  (enable-theme-functions . fontaine-apply-current-preset)
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

;; * CONFIGURATION
;; ** user details

(setq user-full-name "Marc Wenzlawski"
      user-mail-address "marcwenzlawski@gmail.com")

;; (require 'secret)

;; ** secrets

(setq auth-sources '("~/.authinfo.gpg"))
(setopt epa-pinentry-mode 'loopback)


;; ** Emacs

(use-package emacs
  :hook (prog-mode . show-paren-mode)
  (prog-mode . hl-line-mode)
  :custom-face
  (show-paren-match ((t (:underline nil :inverse-video nil))))
  (deault ((t (:family "Fira Code"))))
  (variable-pitch ((t (:family "iA Writer Quattro V"))))
  (fixed-pitch ((t (:family "Fira Code"))))
  :bind
  ("C-z" . nil)
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
  ("C-c <SPC>" . mode-line-other-buffer)
  ("<C-i>" . completion-at-point)
  ("C-j" . reindent-then-newline-and-indent)
  (:map tab-prefix-map
	("h" . tab-bar-mode)
	("s" . tab-switcher))
  (:map help-map
	("W" . man))
  ;;  (:map dired-mode-map
  ;;    ("K" . dired-kill-subdir))
  (:map completion-list-mode-map
	("e" . switch-to-minibuffer))
  :config
  (setq-default fill-column 79
		line-spacing 0.1
		electric-indent-inhibit t
		require-final-newline t
		)

  (setq undo-limit 80000000
	auto-save-default t
	inhibit-compacting-font-caches t
	truncate-string-ellipsis "…"
	shell-file-name (executable-find "fish")
	confirm-kill-emacs 'yes-or-no-p
	redisplay-dont-pause t
	sentence-end-double-space nil
	frame-inhibit-implied-resize t
	scroll-margin 0
	scroll-conservatively 0
	;; frame-title-format '("" "what the %b")
	ns-use-proxy-icon t
	cursor-type t
	blink-cursor-delay 1
	blink-cursor-interval 0.3
	register-preview-delay 0.25
	history-length 100
	initial-scratch-message ";; scratchy scratch"
	prescient-history-length 1000
	tab-always-indent 'complete
	completion-cycle-threshold nil
	abbrev-file-name "~/.emacs.d/abbrev_defs"
	xref-search-program 'ripgrep
	delete-by-moving-to-trash t
	uniquify-buffer-name-style 'forward
	window-combination-resize t
	x-stretch-cursor t
	large-file-warning-threshold 100000000
	show-paren-delay 0
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t
	fit-window-to-buffer-horizontally t
	calendar-latitude '[50 50 north]
	calendar-longitude '[12 55 east]
	)
  (pixel-scroll-precision-mode)
  (delete-selection-mode)
  (fringe-mode '(0 . 0))
  ;; (blink-cursor-mode)
  (recentf-mode)
  (global-auto-revert-mode)
  (push '(lambda (_) (menu-bar-mode -1)) (cdr (last after-make-frame-functions)))
  (add-to-list 'default-frame-alist '(font . "Iosevka-18")))

;; ** CUSTOM FILE

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


;; ** Man

(use-package man
  :bind
  (:map Man-mode-map
	("g" . consult-imenu)))

;; ** editorconfig

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode))

;; ** visual-fill-column

(use-package visual-fill-column
  :straight t
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text nil)
  :config
  (defun my/org-visual-fill-column ()
    (if (and (eq major-mode 'org-mode) (bound-and-true-p org-indent-mode))
	(if (bound-and-true-p visual-fill-column-mode)
	    (setopt visual-fill-column-extra-text-width '(6 . 4))
	  (setopt visual-fill-column-extra-text-width nil)))))

;; ** dtrt-indent

(use-package dtrt-indent
  :straight t
  :custom
  (dtrt-indent-verbosity 0))
;; (add-to-list 'dtrt-indent-hook-mapping-list '(lua-ts-mode lua lua-ts-indent-offset))
;; (dtrt-indent-global-mode))

;; ** gcmh

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))

;; ** tabspaces

(use-package tabspaces
  :straight t)

;; ** outline

(use-package outline
  :straight t
  :bind
  (:map outline-minor-mode-map
	("C-c C-c C-c" . outline-cycle))
  :custom
  (outline-minor-mode-prefix ""))

;; ** openwith

(use-package openwith
  :straight t)

;; ** tab-bar

(use-package tab-bar
  :custom
  (tab-bar-select-tab-modifiers '(super))
  :bind
  (:map tab-bar-mode-map
	("C-)" . tab-recent))
  :config
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; ** exec-path-from-shell

(use-package exec-path-from-shell
  :straight t
  :when (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

;; ** calc

(use-package calc)

(with-eval-after-load 'calc
  (defvar-local calc-trail-buffer-file-name nil
    "Like `buffer-file-name' for calc-trail buffers.")

  (defun calc-trail-save (&optional filename)
    "Save current calc trail buffer.
To be used in `write-contents-functions'.
Append with current prefix arg."
    (interactive "FCalc Trail File: ")
    (unless filename
      (setq calc-trail-buffer-file-name
	    (expand-file-name (setq filename
				    (read-file-name "Calc Trail File: " nil calc-trail-buffer-file-name)))))
    (when (null (derived-mode-p 'calc-trail-mode))
      (user-error "Saving calc trail buffers requires calc-trail-mode"))
    (save-excursion
      (save-restriction
	(widen)
	(let* ((b-trail (progn (goto-char 1) (1+ (line-end-position))))
	       (b (progn (goto-char (max (or (and (use-region-p) (region-beginning)) (point-min)) b-trail))
			 (line-beginning-position)))
	       (e (progn (goto-char (max (or (and (use-region-p) (region-end)) (point-max)) b-trail))
			 (line-end-position))))
	  (write-region b e filename current-prefix-arg)))))

  (defun calc-insert-file (filename)
    "Insert calc-trail file FILENAME at point."
    (interactive "FCalc trail file: ")
    (when (= (line-beginning-position) 1)
      (goto-char (1+ (line-end-position))))
    (goto-char (line-beginning-position
		(if (looking-at "[[:space:]]*$")
		    2
		  1)))
    (let ((inhibit-read-only t))
      (insert-file-contents filename)
      (when (and (null (looking-at "[[:space:]]*$"))
		 (null (looking-back "^[[:space:]]*" (line-beginning-position))))
	(insert "\n"))))

  (defun calc-trail-install-save ()
    "Install `calc-trail-save' in `write-contents-functions' of `calc-trail-mode' buffers."
    (push #'calc-trail-save write-contents-functions)
    (local-set-key (kbd "C-x i") #'calc-insert-file))

  (add-hook 'calc-trail-mode-hook #'calc-trail-install-save)
  (bind-key "C-x C-s" #'calc-trail-save 'calc-mode-map))

;; ** savehist

(use-package savehist
  :custom
  (savehist-additional-variables '(tablist-named-filter kill-ring search-ring regexp-search-ring))
  :init
  (savehist-mode))

;; ** htmlize

(use-package htmlize
  :straight t)

;; ** config profiler esup

(use-package esup
  :straight t)

;; * HELP
;; ** tldr

(use-package tldr
  :straight t
  :custom-face
  (tldr-command-itself ((t (:inherit font-lock-keyword-face :weight bold :background unspecified :foreground "orange"))))
  (tldr-command-argument ((t nil)))
  (tldr-code-block ((t (:foreground unspecified :background unspecified)))))

;; ** devdocs

(use-package devdocs
  :straight t
  :bind (:map help-map ("D" . devdocs-lookup)))

;; ** helpful

(use-package helpful
  :straight t
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
  (eldoc-idle-delay 0.05)
  (eldoc-current-idle-delay 0.05))

;; ** pos-tip

(use-package pos-tip
  :straight t)

;; ** which-key

(use-package which-key
  :straight t
  :custom
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

;; * USER EXPERIENCE
;; ** ace-window

(use-package ace-window
  :straight t
  :custom
  (aw-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i))
  (aw-scope 'frame)
  (aw-reverse-frame-list t)
  (aw-dispatch-alist
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
  :straight t
  :bind
  ("C-T" . avy-goto-char)
  ("C-t" . avy-goto-char-timer)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  :custom
  (avy-background nil)
  (avy-dispatch-alist
   '((?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?h . avy-action-teleport)
     (?u . avy-action-mark)
     (?c . avy-action-copy)
     (?y . avy-action-yank)
     (?f . avy-action-ispell)
     (?z . avy-action-zap-to-char)))
  (avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o)))

;; ** embark

(use-package embark
  :straight t
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
  :straight t
  :after consult embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package dash
  :straight t)
(use-package embark-vc
  :straight t)

;; ** hydra

(require 'setup-hydra)

;; ** undo-fu

(use-package undo-fu
  :straight t)

(use-package undo-fu-session
  :straight t)

;; ** vundo

(use-package vundo
  :straight t)

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
  :straight t)

;; ** bookmark

(use-package bookmark
  :custom
  (bookmark-save-flag 1))

(use-package bookmark+
  :straight (bookmark+))

;; ** scratch

(use-package scratch
  :straight (:host codeberg :repo "emacs-weirdware/scratch" :files ("*.el")))

;; ** exiftool

(use-package exiftool
  :straight t
  :defer t)

;; ** pandoc-mode

(use-package pandoc-mode
  :straight t
  :hook ((text-mode doc-view-mode pdf-view-mode) . pandoc-mode)
  :config
  (bind-key "C-c p" #'pandoc-main-hydra/body 'pandoc-mode-map)
  (bind-key "C-c /" nil 'pandoc-mode-map))

;; ** corfu

;; TODO: Add prescient
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)               ;; Enable auto completion
  (corfu-auto-delay 0.09)
  (corfu-auto-prefix 4)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  (corfu-quit-no-match t) ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-on-exact-match 'show)     ;; Configure handling of exact matches
  (corfu-min-width 40)
  (corfu-max-width 40)
  (corfu-scroll-margin 3)        ;; Use scroll margin
  :bind
  (:map corfu-map
        ;; Option 1: Unbind RET completely
        ("RET" . nil))
  ;; Option 2: Use RET only in shell modes
  ;; ("<return>" . (menu-item "" nil :filter corfu-insert-shell-filter)))

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  ;; :init
  ;; (global-corfu-mode)
  :config
  (use-package corfu-popupinfo
    :hook (corfu-mode . corfu-popupinfo-mode)
    :custom
    (corfu-popupinfo-delay '(0.5 . 0.05)))
  (use-package corfu-history
    :hook (corfu-mode . corfu-history-mode))
  (use-package corfu-info))

(use-package nerd-icons-corfu
  :after corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(with-eval-after-load 'corfu
  (defun corfu-insert-shell-filter (&optional _)
    "Insert completion candidate and send when inside comint/eshell."
    (when (or (derived-mode-p 'eshell-mode) (derived-mode-p 'comint-mode))
      (lambda ()
	(interactive)
	(corfu-insert)
	;; `corfu-send-shell' was defined above
	(corfu-send-shell))))
  
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
		(bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (defun my-corfu-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
    (let ((candidates
           (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
             (if display-sort-func
		 (funcall display-sort-func candidates)
	       candidates))))
      (if corfu-sort-function
          (funcall corfu-sort-function candidates)
	candidates)))

  (setq corfu-sort-override-function #'my-corfu-combined-sort)
  
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
	     completion-cycle-threshold completion-cycling)
	 (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

;; ** prescient

;; (use-package prescient
;;   :straight t)

;; (use-package corfu-prescient
;;   :after prescient
;;   :straight t)

;; (use-package vertico-prescient
;;   :after presciet
;;   :straight t)

;; ** abbrev

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (quietly-read-abbrev-file))

;; ** dabbrev

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

;; ** cape

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet-capf
  :straight (:host github :repo "elken/yasnippet-capf")
  :after cape
  :init
  (defun yasnippet-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
		(cons #'yasnippet-capf
		      completion-at-point-functions)))
  :hook (prog-mode . yasnippet-setup-capf) 
  :config
  (setopt yasnippet-capf-lookup-by 'key))

(defun my/ignore-elisp-keywords (cand)
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defun my/setup-elisp ()
  (setq-local completion-at-point-functions
	      `(,(cape-capf-super
		  #'yasnippet-capf
                  (cape-capf-predicate
                   #'elisp-completion-at-point
                   #'my/ignore-elisp-keywords)
                  #'cape-dabbrev
		  #'tempel-complete)
                cape-file)
	      cape-dabbrev-min-length 5))

(add-hook 'emacs-lisp-mode-hook #'my/setup-elisp)

(defun my/setup-julia ()
  (setq-local completion-at-point-functions
	      `(,(cape-capf-super
		  #'julia-snail-repl-completion-at-point
		  #'tempel-expand
		  #'yasnippet-capf
		  #'julia-mode-latexsub-completion-at-point-around
		  #'julia-mode-latexsub-completion-at-point-before)
		cape-file)))

(add-hook 'julia-snail-mode-hook #'my/setup-julia)


;; ** expand-region

(use-package expand-region
  :straight t
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
  :straight t
  :init
  (marginalia-mode))

;; ** vterm

(use-package vterm
  :straight t
  :bind
  ("C-c t" . vterm)
  ("C-c 4 t" . vterm-other-window)
  :custom
  (vterm-eval-cmds
   '(("find-file" find-file)
     ("find-file-other-window" find-file-other-window)
     ("message" message)
     ("vterm-clear-scrollback" vterm-clear-scrollback)
     ("dired" dired)
     ("man" man)
     ("tldr" tldr)
     ("ediff-files" ediff-files)))
  (vterm-max-scrollback 10000)
  (vterm-shell (executable-find "fish")))

;; ** orderless

(use-package orderless
  :straight t
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
  (move-overlay (bound-and-true-p vertico--candidates-ov) (point-min) (point-min))
  (unless (eq (bound-and-true-p vertico-resize) t)
    (setq lines (nconc (make-list (max 0 (- (bound-and-true-p vertico-count) (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put (bound-and-true-p vertico--candidates-ov) 'before-string string)
    (overlay-put (bound-and-true-p vertico--candidates-ov) 'after-string nil))
  (vertico--resize-window (length lines)))

;; Enable vertico
(use-package vertico
  :straight t
  :custom-face
  ;;(vertico-current ((t (:background "slate"))))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize 'grow-only)
  :init
  (vertico-mode)
  ;; (advice-add #'vertico--display-candidates
  ;; 	      :override #'vertico-bottom--display-candidates)
  :config
  (bind-key "C-c C-n" #'vertico-quick-jump 'vertico-map))


(use-package vertico-multiform
  :straight nil
  :after vertico
  :init
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
	'((consult-ripgrep buffer)
	  (consult-buffer flat))))

;; ** consult

(require 'setup-consult)

;; ** grep

(use-package grep
  :straight t
  :config
  (when (executable-find "rg")
    (setq grep-command "rg --no-heading --line-number --color never %s %s")
    (setq grep-program "rg")))

;; ** rg

(use-package rg
  :straight t
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

(use-package wgrep
  :straight t)

;; ** substitute

(use-package substitute
  :straight t)

;; ** occur-x

(use-package occur-x
  :straight t
  :hook (occur-mode . turn-on-occur-x-mode))

;; ** loccur

(use-package loccur
  :straight (:host codeberg :repo "fourier/loccur")
  :bind
  (:map isearch-mode-map
	("M-s l" . loccus-isearch)))

;; ** dired

(use-package dired
  :bind
  (:map dired-mode-map
	("M-o" . dired-do-open)
	("M-RET" . my/open-current-dir-in-finder))
  :hook (dired-mode . (lambda () (setq truncate-lines t)))
  :config
  (defun my/open-current-dir-in-finder ()
    "Open current directory in Finder."
    (interactive)
    (shell-command "open .")))

(use-package dired-x
  :after dired)

(use-package dired-hacks-utils
  :straight t
  :after dired)

(use-package dired-subtree
  :straight t
  :after dired-hacks-utils)

(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))

(use-package fd-dired
  :straight t)

;; ** magit / git

(use-package magit
  :straight t)

(use-package diff-hl
  :straight t)

(use-package git-gutter
  :straight t)

(use-package vc-git
  :config
  (autoload 'vc-git-root "vc-git"))

;; ** projectile

(use-package project)

(use-package projectile
  :straight t
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :bind
  (:map projectile-command-map ("b" . consult-project-buffer))
  :config
  (setq projectile-project-search-path
	'("~/fun/" "~/fun/web/" "~/fun/python" "~/fun/julia" "~/fun/projects" "~/dotfiles" "~/Dropbox/repos"))
  (projectile-mode))

;; ** flycheck

(use-package flycheck
  :straight t
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-emacs-lisp-check-declare nil)
  (flycheck-emacs-lisp-initialize-packages 'auto)
  (flycheck-emacs-lisp-package-user-dir nil)
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-eglot
  :straight t
  :after flycheck eglot
  :config
  (global-flycheck-eglot-mode 1))

;; ** flycheck ruff

;; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
	      "check"
	      "--output-format=text"
	      (eval (when buffer-file-name
		      (concat "--stdin-filename=" buffer-file-name)))
	      "-")
    :standard-input t
    :error-filter (lambda (errors)
		    (let ((errors (flycheck-sanitize-errors errors)))
		      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
	      (file-name) ":" line ":" (optional column ":") " "
	      (id (one-or-more (any alpha)) (one-or-more digit)) " "
	      (message (one-or-more not-newline))
	      line-end))
    :modes (python-mode python-ts-mode))

  ;; Use something adapted to your config to add `python-ruff' to `flycheck-checkers'
  ;; This is an MVP example:
  (use-package python
    :hook (python-base-mode . (lambda nil (unless (bound-and-true-p org-src-mode)
					    (when (buffer-file-name)
					      (setq-local flycheck-checkers '(python-ruff))
					      (flycheck-mode)))))))

;; ** flycheck zig

(with-eval-after-load 'flycheck
  (flycheck-define-checker zig
    "A zig syntax checker using zig's `ast-check` command."
    :command ("zig" "ast-check" (eval (buffer-file-name)))
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
    :modes (zig-mode zig-ts-mode))
  (add-to-list 'flycheck-checkers 'zig))

;; main.zig:8:8: error: expected ';' after statement


;; ** flyspell

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

(use-package flyspell-correct-avy-menu
  :straight t
  :after flyspell-correct)

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Use `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
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
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it use `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
	(message "Flyspell off")
	(flyspell-mode -1))
                                        ; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;; not being used, as we are not using ispell dicts
(defun my/switch-dictionary()
  "UNUSED. Toggle dictionary language between english and german."
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(use-package flyspell
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

;; ** TODO dape

(use-package dape
  :disabled
  :straight t)

;; ** dictionary

(use-package dictionary
  :custom
  (dictionary-server "dict.org"))

(use-package osx-dictionary
  :straight t)

;; * LANGUAGE TOOLS
;; ** yasnippet

(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  (snippet-mode . (lambda () (setq-local require-final-newline nil)))
  :config
  (setq yas-verbosity 0)
  (use-package yasnippet-snippets
    :straight t)
  
  (yas-reload-all)
  (set-face-attribute 'yas-field-highlight-face nil
		      :inherit 'region :background (modus-themes-get-color-value 'bg-blue-subtle)))

(use-package yankpad
  :straight t
  :disabled
  :init
  (setq yankpad-file "~/.emacs.d/yankpad.org"))

;; ** tempel

(use-package tempel
  :straight t
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

;; (use-package tempel-collection
;;   :straight t
;;   :after tempel)


;; ** quickrun

(use-package quickrun
  :straight t)

;; ** copilot

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :commands copilot-login
  :bind (:map copilot-completion-map ("<C-i>" . copilot-accept-completion))
  :config
  (setq copilot-idle-delay 0.3)
  (add-to-list 'copilot-indentation-alist '(lisp-interaction-mode 2)))

(use-package jsonrpc)
;;  :pin gnu-elpa)

;; ** eglot

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs
	       '((svelte-mode svelte-ts-mode) . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '((zig-mode zig-ts-mode) . ("zls")))
  :hook (eglot-managed-mode . (lambda () (cond ((derived-mode-p 'python-base-mode)
						(add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
					       (t nil))))
  :bind
  (:map eglot-mode-map
	("C-c e f" . eglot-format)
	("C-c e q" . eglot-shutdown)
	("C-c e Q" . eglot-shutdown-all)
	("C-c e l" . eglot-list-connections)
	("C-c e r" . eglot-rename))
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil
		      :bold t :underline nil :background (modus-themes-get-color-value 'bg-yellow-intense)))

(with-eval-after-load 'eglot
  (defun my/eglot-capf ()
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'yasnippet-capf
		       #'tempel-expand
		       #'eglot-completion-at-point
		       #'cape-file))))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))

;; ** tree-sitter

(setopt treesit-language-source-alist
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
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  (zig "https://github.com/maxxnino/tree-sitter-zig")))
(setopt treesit-font-lock-level 3)

(use-package treesit-auto
  :disabled
  :straight t
  :config
  (delete 'c treesit-auto-langs)
  (global-treesit-auto-mode))

;; ** apheleia

(use-package apheleia
  :straight t
  :bind
  (:map prog-mode-map ("C-c f" . apheleia-format-buffer))
  :config
  (with-eval-after-load 'julia-mode
    (push
     '(julia "~/.emacs.d/scripts/julia-format.sh" inplace )
     apheleia-formatters)
    (add-to-list 'apheleia-mode-alist '(julia-mode . julia)))

  (add-to-list 'apheleia-mode-alist '(python-mode . ruff-isort))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff-isort))
  (push
   '(zig-fmt zig-zig-bin "fmt" inplace) apheleia-formatters)

  (add-to-list 'apheleia-mode-alist '(zig-ts-mode . zig-fmt))

  (apheleia-global-mode))

;; ** emmet

(use-package emmet-mode
  :straight t
  :commands (emmet-find-left-bound emmet-transform emmet-reposition-cursor)
  :hook (html-mode . emmet-mode))

;; ** format-all

;; problem with emacs format region
;; but apheleia does not have format region.
(use-package format-all
  :straight t
  :disabled)

;; ** dumb-jump

(use-package dumb-jump
  :straight t
  :custom
  (xref-show-definitions-function 'xref-show-definitions-completing-read)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; ** irony

(use-package irony
  :straight t)
;;:hook (c-ts-base-mode . irony-mode))

(use-package irony-eldoc
  :straight t
  :hook (irony-mode . irony-eldoc))

(with-eval-after-load 'irony
  (defun my/irony-capf ()
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'yasnippet-capf
		       #'irony-completion-at-point
		       #'tempel-expand
		       #'cape-file))))

  (add-hook 'irony-mode-hook #'my/irony-capf))

;; ** polymode

(use-package polymode
  :straight t)

(use-package poly-org
  :straight t
  :hook (polymode-init-host . (lambda () (if (eq major-mode 'org-mode) (org-modern-mode 1))))
  :config
  (setq auto-mode-alist (delete '("\\.org\\'" . poly-org-mode) auto-mode-alist)))

;; * LANGUAGE MODES
;; ** lisp

(use-package lisp-mode
  :hook (lisp-data-mode . electric-pair-mode))

;; ** elisp

(use-package elisp-mode
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
	("M-o" . consult-imenu)))

(use-package pyenv-mode
  :straight t
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
  :straight t
  :bind
  (:map python-ts-mode-map
	("C-c C-b" . poetry)))

(use-package python-pytest
  :straight t
  :bind
  (:map python-ts-mode-map
	("C-c C-n" . python-pytest-dispatch)))

;; ** jupytyer

(use-package jupyter
  :straight t)


;; ** ess

(use-package ess
  :straight t
  :defer t
  :hook (comint-mode . (lambda () (toggle-truncate-lines -1)))
  :custom
  (ess-eval-visibly 'nowait)
  (ess-use-company 'nil)
  (ess-R-font-lock-keywords
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
     (ess-R-fl-keyword:F&T . t)))
  (ess-history-directory "~/.ess"))

(use-package comint
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (comint-move-point-for-output 'others)
  (comint-buffer-maximum-size 4096))

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
  "Add pdf(tmpfile) and dev.off() to R command."
  (let*  (
	  (newc (concat "pdf('" fname "')\n" rcomm  "\n dev.off()"))
	  )
    (eval newc)
    )
  )

(defun rutils-plot-region-or-paragraph()
  "Execute region or paragraph and save tmp plot to pdf. Then open windows to show pdf."
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
  :straight t
  :hook (julia-ts-mode . (lambda nil (progn (apheleia-mode -1) (setq-local eglot-connect-timeout 300))))
  :mode "\\.jl\\'")

(use-package julia-ts-mode
  :disabled
  :straight t
  :hook (julia-ts-mode . (lambda nil (progn (apheleia-mode -1) (setq-local eglot-connect-timeout 300))))
  :mode "\\.jl$")

(use-package julia-snail
  :straight t
  :bind
  (:map julia-snail-mode-map
	("C-c f" . julia-snail/formatter-format-buffer))
  :custom
  (julia-snail-popup-display-eval-results ':command)
  (julia-snail-repl-display-eval-results nil)
  (julia-snail-multimedia-enable t)
  :config
  (setq-default julia-snail-extensions '(repl-history formatter ob-julia))
  (add-to-list 'display-buffer-alist
               '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))
  :hook (julia-mode . julia-snail-mode)
  (julia-snail-mode . (lambda () (apheleia-mode -1))))

;;   :config
;;   (add-to-list 'display-buffer-alist
;; 	       '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))

(use-package eglot-jl
  :straight t
  :after eglot
  :custom
  (eglot-jl-language-server-project "~/.julia/environments/v1.10")
  :config
  (eglot-jl-init))

(use-package ob-julia-vterm
  :straight t
  :disabled
  :config
  (add-to-list 'org-babel-load-languages '(julia-vterm . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; ** markdown

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . visual-line-mode))

;; ** typst

(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :custom
  (typst-ts-mode-watch-options "--open"))

;; ** fish

(use-package fish-mode
  :straight t
  :config
  (setq fish-enable-auto-indent t))
;; ** lua

(use-package lua-mode
  :straight t
  :config
  (setq lua-indent-level 3))

(use-package lua-ts-mode
  :straight (:host sourcehut :repo "johnmuhl/lua-ts-mode" :files ("*.el"))
  :disabled
  :hook (lua-ts-mode . (lambda nil (setq tab-width 3)))
  :config
  (setq lua-ts-indent-offset 3))

;; ** cc-mode

(use-package cc-mode
  :hook (awk-mode . (lambda nil (setq tab-width 4)))
  (c-mode . hs-minor-mode)
  (c-mode . semantic-mode)
  :bind
  (:map c-mode-base-map
	("C-c C-t" . comment-region)))

;; ** css

(use-package css-mode
  :hook ((css-mode css-ts-mode) . (lambda nil (setq tab-width 2)))
  :config
  (setq css-indent-offset 2))

;; ** nix

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package nix-buffer
  :straight t
  :after nix-mode)

;; ** bibtex

;; {{ firstCreator suffix=" - " }}{{ year suffix=" - " }}{{ title truncate="100" }}

(use-package bibtex-completion
  :when (package-installed-p 'org-ref)
  :straight (bibtex-completion :type git :flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :host github :repo "tmalsburg/helm-bibtex")
  :custom
  (bibtex-completion-bibliography '("~/zotero/bibtex-export.bib" "~/cat.bib"))
  (bibtex-completion-library-path '("~/zotero/storage"))
  (bibtex-completion-notes-path "~/Dropbox/Org/articles.org")
  (bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n")
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-additional-search-fields '("keywords"))
  (bibtex-completion-display-formats
   '((article       . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
     (inbook        . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
     (incollection  . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (inproceedings . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
     (t             . "${=has-pdf=:1} ${=has-note=:1} ${year:4} ${author:36} ${title:*}")))

  (bibtex-completion-pdf-symbol "")
  (bibtex-completion-notes-symbol "")
  (bibtex-completion-notes-template-one-file "\n* ${author-or-editor} (${year}): ${title}\n:PROPERTIES:\n:Custom_ID: ${=key=}\n:END:\n\n"))

(with-eval-after-load 'bibtex-completion
  (defun my/org-ref-format-citation (keys)
    "Format ebib references for keys in KEYS."
    (s-join ", "
	    (--map (format "cite:&%s" it) keys)))

  (add-to-list 'bibtex-completion-format-citation-functions '(org-mode . my/org-ref-format-citation)))

(use-package bibtex
  :when (package-installed-p 'org-ref)
  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5))

;; ** zig

(use-package zig-ts-mode
  :mode ("\\.zig\\'" . zig-ts-mode))

;; ** go

(use-package go-ts-mode
  :custom
  (go-ts-mode-indent-offset 4)
  :hook
  (go-ts-mode . (lambda () (setq tab-width 4)))
  )

(use-package gotest
  :straight t
  :bind
  (:map go-ts-mode-map
	("C-c C-t f" . go-test-current-file)
	("C-c C-t t" . go-test-current-test)
	("C-c C-t p" . go-test-current-project)
	("C-c C-t c" . go-test-current-coverage)
	("C-c C-t r" . go-test-current-test-cache)
	("C-c C-t b" . go-test-current-file-benchmarks)
	("C-c C-t B" . go-test-current-project-benchmarks)))

;; * ORG

(require 'setup-org)


;; * APPLICATIONS
;; ** smudge spotify

(require 'setup-smudge)

;; ** citar

(require 'setup-citar)

;; ** calibre

(require 'setup-calibre)

;; ** elfeed

(use-package elfeed
  :straight t
  :disabled)

(use-package elfeed-org
  :disabled
  :straight t
  :after elfeed
  :commands elfeed-org
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/feeds.org"))
  (setopt elfeed-search-title-max-width 100)
  (elfeed-org))

;; ** notmuch

(use-package notmuch
  :straight t)

;; ** eww

(defun my/scroll-up-half ()
  "Scroll up half the window height."
  (interactive)
  (scroll-up-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(defun my/scroll-down-half ()
  "Scroll down half the window height."
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

;; ** shr

(use-package shr
  :bind
  (:map shr-map
	("u" . nil))
  :config
  (setq shr-max-image-proportion 0.4))

;; ** denote

(defun my/denote-rename-buffer ()
  "Rename the buffer to the title of the current note."
  (interactive)
  (denote-rename-buffer))

(use-package denote
  :straight t
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
  ("C-c n z" . denote-signature)
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
  (denote-rename-buffer-mode))

(use-package denote-org-extras
  :after denote)

(use-package denote-explore
  :straight t
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
  "Center the text in visual column mode."
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
  "Set up the nov mode."
  ;; (my/nov-font-setup)
  (hl-line-mode -1)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (variable-pitch-mode 1))

(defun my/toggle-header-line ()
  "Toggle the display of the header line."
  (interactive)
  (if nov-header-line-format
      (setq nov-header-line-format nil)
    (setq nov-header-line-format "%t: %c"))
  (nov-render-document))

(defun my/toggle-cursor-display ()
  "Toggle between displaying a bar and no cursor."
  (interactive)
  (if cursor-type
      (setq cursor-type nil)
    (setq cursor-type 'bar)))

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  :bind
  (:map nov-mode-map
	("j" . (lambda () (interactive) (scroll-up 1)))
	("k" . (lambda () (interactive) (scroll-down 1)))
	("z" . visual-fill-column-mode)
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

(use-package esxml
  :straight t)

;; ** ebib

(use-package ebib
  :disabled
  :straight t
  :config
  (setq ebib-preload-bib-files '("~/Zotero/bibtex-export.bib")))

;; ** speed-type

(use-package speed-type
  :straight t)

;; ** fireplace

(use-package fireplace
  :straight t)

;; ** gptel

(use-package gptel
  :straight t
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
  "Show the Anki app."
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

(defun my/background-pdf-view-refresh (_)
  "Refresh the themed minor mode in pdf-view."
  (cl-loop for buf in (buffer-list)
	   collect
	   (with-current-buffer buf
	     (when (eq major-mode 'pdf-view-mode)
	       (my/pdf-view-themed-minor-mode-refresh)))))

(defun my/pdf-view-themed-minor-mode-refresh ()
  "Refresh the themed minor mode in pdf-view."
  (interactive)
  (pdf-view-themed-minor-mode 1))

(defun my/pdf-view-current-page ()
  "Show the current page number in the minibuffer."
  (interactive)
  (message "%d/%d" (pdf-view-current-page) (pdf-info-number-of-pages)))

(defun my/pdf-view-open-externally ()
  "Open the current pdf in an external viewer."
  (interactive)
  (shell-command (concat "open '" buffer-file-name "'")))

(use-package pdf-view
  :after pdf-tools
  :custom
  (pdf-view-resize-factor 1.05)
  (pdf-view-display-size 'fit-page)
  (pdf-view-midnight-colors '("#E2E2E2" . "#070707"))
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :bind
  (:map pdf-view-mode-map
	("C-c C-o" . my/pdf-view-open-externally)
	("C-c C-r r" . my/pdf-view-themed-minor-mode-refresh)
	("c" . my/pdf-view-current-page)
	("o" . pdf-outline)
	("d" . dictionary-search)
	("D" . osx-dictionary-search-input)
	("C-c C-n" . org-noter))
  :config
  (add-to-list 'display-buffer-alist '("\\`\\*Outline.*\\*" nil (window-width . 0.3))))

(use-package saveplace-pdf-view
  :straight t
  :config
  (save-place-mode 1))

;; ** pdf-tools

(use-package pdf-tools
  :straight t
  :defer 2
  :hook (pdf-outline-buffer-mode . visual-line-mode)
  :config
  (pdf-tools-install :no-query)
  (use-package pdf-occur))

;; ** pdf-annot

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

;; ** image

(use-package image
  :bind
  (:map image-slice-map
	("C-<mouse-4>" . nil)
	("C-<mouse-5>" . nil)
	("C-<wheel-up>" . nil)
	("C-<wheel-down>" . nil)))
;; ** gnuplot

(use-package gnuplot-mode
  :straight t)
(use-package gnuplot
  :straight t)

;; ** ebdb

(use-package ebdb
  :straight t)

;; ** ledger-mode

(use-package ledger-mode
  :straight t
  :hook
  (ledger-mode . (lambda ()
		   (setq-local tab-always-indent 'complete)
		   (setq-local completion-cycle-threshold t)
		   (setq-local ledger-complete-in-steps t))))
;; :hook
;; (ledger-mode . auto-revert-tail-mode)
;; (ledger-mode-hook . (lambda () (setq tab-width 4)))
;; :bind
;; (:map ledger-mode-map
;; 	("M-0" . (lambda () (interactive) (set-selective-display (* tab-width 0))))
;; 	("M-1" . (lambda () (interactive) (set-selective-display (* tab-width 1)))))
;; :mode
;; ("\\.rules\\'" . conf-mode)
;; ("\\.\\(h?ledger\\|journal\\|j\\)\\'" . ledger-mode)
;; :custom
;; (ledger-binary-path "~/.emacs.d/hledger.sh")
;; (ledger-mode-should-check-version nil)
;; (ledger-init-file-name " ")
;; (ledger-post-amount-alignment-column 64)
;; (ledger-highlight-xact-under-point nil)
;; (ledger-report-links-in-register nil)
;; (ledger-report-auto-width nil)
;; (ledger-report-use-native-highlighting nil)
;; :config
;; (defun highlight-negative-amounts nil (interactive)
;; 	 (highlight-regexp "\\(\\$-\\|-\\$\\)[.,0-9]+" (quote hi-red-b))))

;; ** w3m

(use-package w3m
  :straight t)

(use-package w3m-search
  :after w3m
  :custom
  (w3m-search-default-engine "duckduckgo")
  :config
  (add-to-list 'w3m-search-engine-alist '("duckduckgo" "https://html.duckduckgo.com/html/?q=%s")))

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
  :commands (Lorem-ipsum-insert-sentences Lorem-ipsum-insert-list Lorem-ipsum-insert-paragraphs))

;; ** svelte-ts-mode

(use-package svelte-ts-mode
  :disabled
  :commands (svelte-ts-mode)
  :mode "\\.svelte\\'"
  :hook
  (svelte-ts-mode . (lambda () (apheleia-mode -1)))
  (svelte-ts-mode . (lambda () (setq tab-width 2)))
  :custom-face
  (font-lock-bracket-face ((t (:foreground "tan3")))))

;; * LOCAL-VARIABLES

;; https://stackoverflow.com/questions/76388376/emacs-org-encrypt-entry-hangs-when-file-is-modified
;; DO NOT USE THIS WITH SYMMETRICALLY ENCRYPTED FILES.
;; MAY CAUSE FILE CORRUPTION.
(fset 'epg-wait-for-status 'ignore)

;; Local Variables:
;; outline-regexp: " *;; \\*+"
;; page-delimiter: " *;; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-body)
;; eval:(flycheck-mode -1)
;; coding: utf-8-unix
;; End:
