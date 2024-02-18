  (setq warning-minimum-level :emergency)

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

  ;; Disable the damn thing by making it disposable.
  (setq custom-file (make-temp-file "emacs-custom-"))

  (setq disabled-command-function nil)

;; Always start with *scratch*
;;(setq initial-buffer-choice t)

  (mapc
   (lambda (string)
     (add-to-list 'load-path (locate-user-emacs-file string)))
   '("site-lisp" "mw-emacs-modules"))

  ;;;; Packages

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

  (setq custom-safe-themes t)

  (setq custom-safe-themes t)

  (add-to-list 'ns-system-appearance-change-functions 'mw/apply-theme-change)
  ;; (add-to-list 'after-make-frame-functions '(lambda (_)
  ;; (my/apply-theme-change ns-system-appearance))) ;; DOES NOT WORK
  (push '(lambda (_) (mw/apply-theme-change ns-system-appearance)) (cdr (last after-make-frame-functions)))
  (use-package ef-themes)
  (use-package color-theme-modern)

  (defun mw/theme-default-light ()
    "Set the default theme to light"
    (interactive)
    (load-theme 'modus-operandi-tinted t))

  (defun mw/theme-default-dark ()
    "Set the default theme to dark"
    (interactive)
    (load-theme 'modus-vivendi-tritanopia t))

  (defun mw/apply-theme-change (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (pcase appearance
      ('light (mw/theme-default-light))
      ('dark  (mw/theme-default-dark))))

  (use-package modus-themes
    :config
    (setq modus-themes-common-palette-overrides 
      '((bg-mode-line-active bg-dim)
        (fg-mode-line-active fg-dim)
        (border-mode-line-active bg-dim)
	  (bg-mode-line-inactive bg-main)
	  (border-mode-line-inactive unspecified)))
    (setq modus-themes-mode-line 'borderless))

  (use-package poet-theme
    :config
    (setq poet-theme-variable-pitch-multiplier 1.6)
    (setq poet-theme-variable-headers nil))

  (add-to-list 'default-frame-alist '(font . "Iosevka Comfy-18"))

  (defun mw/writeroom-mode-hook ()
    "Custom behaviours for `writeroom-mode'."
    (if writeroom-mode
        (progn (centered-cursor-mode 1)
               (display-line-numbers-mode 0))
      (centered-cursor-mode 0)))

  (use-package writeroom-mode
    :hook (writeroom-mode . mw/writeroom-mode-hook))
  (use-package centered-cursor-mode)

(defun mw/spacious-padding-reset ()
  "reset the spacious padding and modeline formats"
  (interactive)
  (spacious-padding-mode 1))

(use-package spacious-padding
  :config
  (spacious-padding-mode))

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

(use-package default-text-scale
  :bind
  (:map default-text-scale-mode-map
  ("s-+" . default-text-scale-increase)
  ("s-_" . default-text-scale-decrease))
  :config
  (default-text-scale-mode))

(use-package rainbow-delimiters
  :hook prog-mode)

  (setq user-full-name "Marc Wenzlawski"
        user-mail-address "marcwenzlawski@gmail.com")

(use-package emacs
  :config
  (setq undo-limit 80000000)
  (setq auto-save-default t)
  (setq inhibit-compacting-font-caches t)
  (setq truncate-string-ellipsis "…")
  (setq shell-file-name (executable-find "zsh"))
  (setq confirm-kill-emacs 'yes-or-no-p)
  (setq redisplay-dont-pause t)
  (setq line-spacing 0.15)
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
  (setq fill-column 78)
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
  (pixel-scroll-precision-mode)
  (delete-selection-mode)
  (fringe-mode '(0 . 0))
  (blink-cursor-mode)
  (recentf-mode)
  (push '(lambda (_) (menu-bar-mode -1)) (cdr (last after-make-frame-functions)))
  :bind
  ("C-x C-l" . nil)
  ("C-x C-S-l" . downcase-region)
  ("C-c o" .  occur)
  ("C-x M-k" . kill-this-buffer)
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
  ;;	("K" . dired-kill-subdir))
  (:map completion-list-mode-map
	("e" . switch-to-minibuffer)))

  (use-package editorconfig
    :config
    (editorconfig-mode 1))

(use-package gcmh
  :config
  (gcmh-mode 1))

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

(use-package tabspaces)

  (use-package avy
    :bind
    ("C-T" . avy-goto-char)
    ("C-t" . avy-goto-char-2)
    ("M-g f" . avy-goto-line)
    ("M-g w" . avy-goto-word-1)
    ("M-g e" . avy-goto-word-0)
    :config
    (setq avy-background nil)
    (setq avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o)))

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

(use-package vterm
  :bind
  ("C-c t" . vterm)
  :config
  (setq vterm-shell "/usr/local/bin/fish"))

  (use-package hydra)

(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode))
  (use-package osx-trash
    :config
    (osx-trash-setup)))

  (defun mw/highlight-visual-line ()
    (save-excursion
      (cons (progn (beginning-of-visual-line) (point))
            (progn (end-of-visual-line) (point)))))
  (setq hl-line-range-function 'mw/highlight-visual-line)

  (use-package openwith)

  (use-package undo-fu)

  (use-package undo-fu-session)

  (use-package vundo)

  (use-package hl-todo)

  (use-package exiftool
    :defer t)

(use-package bookmark+
  :straight (bookmark+))

(setq bookmark-save-flag 1)

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ("C-h C-h" . helpful-at-point)
  ("C-h F" . helpful-function))

(use-package tab-bar
  :custom
  (tab-bar-select-tab-modifiers '(super))
  :config
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

(use-package scratch
    :straight (:host codeberg :repo "emacs-weirdware/scratch" :files ("*.el")))

(use-package transpose-frame
  :straight (:host github :repo "emacsorphanage/transpose-frame"))

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

(use-package hide-mode-line
  :bind (:map help-map ("t m" . hide-mode-line-mode)))

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
  (use-package cape)

(defun mw/sort-by-length (elements)
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
(bind-key "e" #'switch-to-minibuffer 'completion-list-mode-map)
(bind-key "<return>" #'minibuffer-force-complete-and-exit 'minibuffer-mode-map)
(bind-key "C-<return>" #'minibuffer-tcomplete-and-exit 'minibuffer-mode-map)

(use-package mct
  :demand t
  :config
  (setq mct-hide-completion-mode-line t)
  ;; The blocklist and passlist accept either commands/functions or
  ;; completion categories.
  (setq mct-completion-blocklist '(notmuch-mua-new-mail notmuch-mua-prompt-for-sender))
  (setq mct-completion-passlist
        '( consult-buffer consult-location embark-keybinding consult-notes consult-fd
	   citar-denote-dwim citar-open citar-denote-open-note consult-project-buffer
	   consult-buffer-other-tab projectile-find-file
	   projectile-find-file-other-frame
	   projectile-find-file-other-window projectile-switch-to-buffer
	   projectile-switch-to-buffer-other-window
	   projectile-switch-to-buffer-other-frame
	   projectile-recentf org-cite-insert citar-dwim
           imenu select-frame-by-name switch-to-buffer))
  (setq mct-remove-shadowed-file-names t)
  (setq mct-completion-window-size (cons #'mct-frame-height-third 1))
  (setq mct-persist-dynamic-completion t)
  (setq mct-live-completion t)
  (setq mct-minimum-input 5)
  (setq mct-live-update-delay 0.0)
  (setq completions-sort #'mct-sort-by-alpha-length)

  (mct-mode 1))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :after org
  :config
  (consult-customize consult-notes :preview-key "M-.")
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
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
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
(use-package consult-flycheck)

(use-package consult-recoll
  :after citar
  :config
  (setq exec-path (append exec-path '("/usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS/")))
  (consult-recoll-embark-setup))

(defun consult-notes-my-embark-function (cand)
  "Do something with CAND"
  (interactive "fNote: ")
  (message cand))

(defun mw/consult-notes--on-file (file)
  (let ((consult-notes-org-headings-files (list file)))
    (consult-notes)))

(defun mw/consult-notes--menu ()
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

(defun mw/consult-notes-org-insert-link ()
  (interactive)
  (let ((file (mw/consult-notes--menu)))
    (if (stringp file)
        (progn (if (equal major-mode 'org-mode)
                   (progn (org-mark-ring-push)
                          (mw/consult-notes--on-file file)
                          (org-store-link nil t)
                          (org-mark-ring-goto)
                          (org-insert-all-links nil "" " "))
                 (mw/consult-notes--on-file file))))))


(defun mw/consult-notes-menu ()
  (interactive)
  (let ((file (mw/consult-notes--menu)))
    (if (stringp file)
        (progn
          (if (equal major-mode 'org-mode) (org-mark-ring-push))
          (mw/consult-notes--on-file file)
          (org-narrow-to-subtree)
          (org-fold-show-subtree)))))

(use-package consult-notes
  :after consult denote
  :bind
  ("C-c n o" . consult-notes)
  ("C-c n X" . consult-notes-search-in-all-notes)
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

(use-package eglot
  :bind
  (:map eglot-mode-map
	("C-c e q" . eglot-shutdown)
	("C-c e Q" . eglot-shutdown-all)
	("C-c e l" . eglot-list-connections))
  :custom-face
  (eglot-highlight-symbol-face ((t (:background "LightSkyBlue4")))))
(use-package eglot-jl)

(use-package pyenv-mode
  :init
  (setq pyenv-mode-map
	(let ((map (make-sparse-keymap)))
	  map))
  :hook python-ts-mode python-mode
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
	("C-c C-n" . python-pytest-dispatch)))

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

  (setq eglot-jl-language-server-project "~/.julia/environments/v1.9")

  (use-package vterm)

  (use-package julia-snail
    :config
    (add-to-list 'display-buffer-alist
                 '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))
    (setq split-height-threshold 15)
    (setq julia-snail-repl-display-eval-results t)
    (setq julia-snail-multimedia-enable t)
    (setq julia-snail-extensions '(repl-history formatter))
    :hook (julia-mode . julia-snail-mode))

(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :custom
  (typst-ts-mode-watch-options "--open"))

  (use-package flycheck)

  (use-package flyspell-correct
    :after flyspell
    :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

  (use-package flyspell-correct-avy-menu
    :after flyspell-correct)

  (use-package consult-flyspell
    :bind (:map flyspell-mode-map ("C-<" . consult-flyspell))
    :config
    ;; default settings
    (setq consult-flyspell-select-function 'flyspell-correct-at-point
          consult-flyspell-set-point-after-word t
          consult-flyspell-always-check-buffer nil))

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
  (defun mw/switch-dictionary()
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

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (regex "https://github.com/tree-sitter/tree-sitter-regex")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (r "https://github.com/r-lib/tree-sitter-r")
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

(use-package format-all
  :commands format-all-mode
  :hook prog-mode
  :bind
  (:map prog-mode-map
	("C-c f" . format-all-region-or-buffer))
  :config
  (setq-default format-all-formatters '(("Python" (black)))))

  (use-package eldoc
    :config
    (setq eldoc-current-idle-delay 0.3))
  (use-package pos-tip)

(use-package yasnippet
  :init
  (use-package yasnippet-snippets)
  (yas-reload-all)
  (setq yas-minor-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "s") 'yas-insert-snippet)
	  (define-key map (kbd "n") 'yas-new-snippet)
	  (define-key map (kbd "v") 'yas-visit-snippet-file)
	  map))
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind-keymap ("C-c s" . yas-minor-mode-map))

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

  (use-package quickrun)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook prog-mode
  :commands copilot-login
  :bind (:map copilot-completion-map ("<tab>" . copilot-accept-completion))
  :config
  (setq copilot-idle-delay 0.3))

(use-package jsonrpc
  :pin gnu-elpa)

(use-package grep
  :config
  (setq grep-program "rg"))

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

  (use-package wgrep)

  (use-package substitute)

  (use-package occur-x
    :hook (occur-mode . turn-on-occur-x-mode))

(use-package loccur
  :straight (:host codeberg :repo "fourier/loccur")
  :bind
  (:map isearch-mode-map
	("M-s l" . loccus-isearch)))

(use-package diredfl)
(use-package fd-dired)
(use-package dired-rsync)

  (use-package magit)

  (use-package diff-hl)

  (use-package git-gutter)

  (use-package nerd-icons
    ;; :custom
    ;; The Nerd Font you want to use in GUI
    ;; "Symbols Nerd Font Mono" is the default and is recommended
    ;; but you can use any other Nerd Font if you want
    ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
    )

(defun mw/org-setup-hook ()
  "Setup org mode hook"
  (display-line-numbers-mode 0)
  ;;(smartparens-mode 0)
  ;;(git-gutter-mode 0)
  (auto-fill-mode 1)
  (setq fill-column 78))

(use-package org
  :pin manual
  ;; :custom
  ;; (display-buffer-alist
  ;;  (append display-buffer-alist
  ;; 	   '(("^\\(CAPTURE-.+\\)$\\|\\*\\(?:Capture\\|Org Select\\)\\*"
  ;; 	      (display-buffer-below-selected display-buffer-at-bottom)
  ;; 	      (inhibit-same-window . t)
  ;; 	      (window-height . )))))
  :config
  (setq org-directory "~/Dropbox/Org/")
  (setq org-agenda-files '("projects.org" "daily.org" "refile.org" "future.org"))
  ;;(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING") "DONE"))
  (setq org-hide-emphasis-markers t)
  (setq org-latex-compiler "xelatex")
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 2)))
  (setq org-ellipsis "↴")
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
        ("C-c e" . org-emphasize))
  :custom-face
  (org-document-title ((t (:height 1.7))))
  )


;; (use-package ob-shell
;;   :after org
;;   :config
;;   (setq org-babel-default-header-args:sh '((:results . "output")))
;;   (setq org-babel-default-header-args:shell '((:results . "output"))))

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

(when (eq system-type 'darwin)
  (defun mw/org-mac-link-applescript-librewolf-get-frontmost-url ()
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

  (defun mw/org-mac-link-librewolf-get-frontmost-url ()
    "Get the link to the frontmost window of the LibreWolf.app."
    (interactive)
    (message "Applescript: Getting Firefox url...")
    (org-mac-link-paste-applescript-links (mw/org-mac-link-applescript-librewolf-get-frontmost-url)))

  (defun mw/org-mac-link-librewolf-insert-frontmost-url ()
    "Insert the link to the frontmost window of the LibreWolf.app."
    (interactive)
    (insert (mw/org-mac-link-librewolf-get-frontmost-url)))

  (defun mw/org-mac-link-get-link (&optional beg end)
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
              ("l" "ibrewolf" mw/org-mac-link-librewolf-insert-frontmost-url ,org-mac-link-librewolf-app-p)
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
	  ("C-c L" . mw/org-mac-link-get-link))))

(use-package org-noter
  :bind
  (:map org-noter-doc-mode-map ("q" . nil))
  (:map pdf-view-mode-map ("C-c C-n" . org-noter))
  (:map org-mode-map
        ("C-c C-x n n" . org-noter)
        ("C-c C-x n k" . org-noter-kill-session)
        ("C-c C-x n s" . org-noter-create-skeleton))
  :config
  (add-to-list 'org-noter-notes-search-path "/Users/mw/Library/CloudStorage/Dropbox/Org")
  (setq org-noter-default-notes-file-names '("noter.org")
        org-noter-always-create-frame nil
        org-noter-auto-save-last-location t
        org-noter-doc-split-fraction '(0.5 . 0.5)
        org-noter-kill-frame-at-session-end nil
        org-noter-separate-notes-from-heading t))

(use-package org-ql)

(use-package org-web-tools)

(use-package notmuch)

(use-package elfeed)
(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/feeds.org"))
  (setq elfeed-search-title-max-width 100)
  (elfeed-org))

(use-package eww
  :bind
  ("C-c w" . eww)
  :config
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil))
(use-package shr
  :config
  (setq shr-max-image-proportion 0.4))

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h nil)
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

(defun mw/denote-rename-buffer ()
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
  ("C-c n f r" . mw/denote-rg-search)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter)
  ("C-c n C-r" . mw/denote-rename-buffer))

(defun mw/denote-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep denote-directory)))

(defun mw/center-reading-mode ()
  "Center the text in visual column mode"
  (interactive)
  (visual-fill-column-mode))

;; TODO make this respeatable, and work with n argument
(defun mw/mark-whole-sentence ()
  "Mark the whole sentence the cursor is in."
  (interactive)
  (backward-sentence)
  (mark-end-of-sentence nil))

;; (defun mw/nov-font-setup ()
;;   (face-remap-add-relative 'variable-pitch :family "ETBembo"))

(defun mw/nov-mode-setup ()
  "Set up the nov mode"
  ;; (mw/nov-font-setup)
  (hl-line-mode -1)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (variable-pitch-mode 1))

(defun mw/toggle-header-line ()
  "Toggle the display of the header line"
  (interactive)
  (if nov-header-line-format
      (setq nov-header-line-format nil)
    (setq nov-header-line-format "%t: %c"))
  (nov-render-document))

(defun mw/toggle-cursor-display ()
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
  ("m s" . mw/mark-whole-sentence)
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
  ("C-c t" . mw/toggle-header-line)
  ("C-c v" . visual-line-mode)
  ("C-c c" . mw/toggle-cursor-display)
  ("C-c b" . org-noter))
  :hook (nov-mode . mw/nov-mode-setup))
(use-package esxml)

(defun mw/refresh-calibre-bib ()
  (interactive)
  (shell-command "calibredb catalog ~/cat.bib --fields=title,authors,formats,id,isbn,pubdate,tags,uuid,identifiers" ))

(use-package calibredb
  :bind
  ("C-c j c" . calibredb)
  ("C-c j C-c" . mw/refresh-calibre-bib)
  ("C-c j C" . calibredb-consult-read)
  :config
  (setq calibredb-root-dir "~/Dropbox/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-id-width 5)
  (setq calibredb-library-alist '(("~/Dropbox/Calibre Library"))))

(defun mw/citar-toggle-multiple ()
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
	("x" . mw/citar-toggle-multiple)
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
  (citar-denote-subdir "citar")
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :init
  (citar-denote-mode 1)
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

(use-package ebib
  :config
  (setq ebib-preload-bib-files '("~/Zotero/bibtex-export.bib")))

(use-package speed-type)

(use-package fireplace
  :bind ("C-c j f" . fireplace))

(use-package gptel)

(defun mw/pdf-view-themed-minor-mode-refresh ()
  (interactive)
  (pdf-view-themed-minor-mode 1))

(defun mw/pdf-view-current-page ()
  (interactive)
  (message "%d/%d" (pdf-view-current-page) (pdf-info-number-of-pages)))

(defun mw/pdf-view-open-externally ()
  (interactive)
  (shell-command (concat "open '" buffer-file-name "'")))

(use-package pdf-view
  :after pdf-tools
  :config
  (setq pdf-view-resize-factor 1.05)
  :mode "\\.pdf\\'"
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :bind
  (:map pdf-view-mode-map
  ("C-c d" . mw/pdf-view-open-externally)
  ("C-c C-r r" . mw/pdf-view-themed-minor-mode-refresh)
  ("c" . mw/pdf-view-current-page)
  ("C-c C-n" . org-noter)))

(use-package saveplace-pdf-view
  :config
  (save-place-mode 1))

(use-package pdf-tools
  :config
  (pdf-tools-install :no-query))

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

(defun mw/launch-note (&optional initial-input key)
  (select-frame-set-input-focus (selected-frame))
  (set-frame-size (selected-frame) 80 15)
  (set-frame-name "org-capture")
  (add-hook 'org-capture-after-finalize-hook 'mw/post-org-launch-note)
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
(defun mw/remove-launch-note-hook ()
  (interactive)
  (remove-hook 'org-capture-after-finalize-hook 'mw/post-org-launch-note))

(defun mw/post-org-launch-note ()
  (mw/remove-launch-note-hook)
  (delete-frame))
