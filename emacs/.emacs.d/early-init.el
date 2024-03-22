;;; -*- lexical-binding: t; no-byte-compile: t -*-
;;; early-init.el --- Early Init File
;; NOTE: early-init.el is generated from config.org.
;; Please edit that file instead.

(add-to-list 'initial-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'initial-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(width . 140))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("what the %b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable GUI elements
(menu-bar-mode -1)
(add-hook 'after-init-hook (lambda nil (menu-bar-mode -1)))
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
                  file-name-handler-alist prot-emacs--file-name-handler-alist
                  vc-handled-backends prot-emacs--vc-handled-backends)))

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;;; early-init.el ends here

;; This is bound to MacOS. In the future, there should be an OS
;; agnostic way of calling these functions. This would be handled by
;; making a boolean checking for OS type at startup and then assigning
;; respective variables to the desktop commands. That way we won't
;; have to constantly do the checking for OS, just do it once.
(defun mw/theme-gsettings-dark-p ()
  "Return non-nil if defaults (MACOS) has a dark theme.
Return nil if the OS is not darwin"
  (if (eq system-type 'darwin)
      (string-match-p
       "Dark"
       (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))

(defun mw/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call
`prot-emacs-re-enable-frame-theme'."
  (when (mw/theme-gsettings-dark-p)
    (set-face-attribute 'default nil :background "#000000"
                        :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000"
                        :foreground "#ffffff" :box 'unspecified)))

(setq mode-line-format nil)
(mw/avoid-initial-flash-of-light)
