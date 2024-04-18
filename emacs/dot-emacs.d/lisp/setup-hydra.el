;;; setup-hydra.el --- Hydra setup
;;; Commentary:
;;; Code:

(use-package hydra
  :straight t)

(use-package pretty-hydra
  :after hydra
  :straight t
  :bind
  ("C-`" . my/hydra-launch-menu/body)
  (:map outline-minor-mode-map
	("C-c <tab>" . my/hydra-outline/body))
  (:map help-map
	("t" . nil)
	("t" . my/hydra-toggle-menu/body))
  :config
  (with-eval-after-load 'emacs
;;; Launch Menu
    (defvar my/hydra-launch-menu)
    (pretty-hydra-define my/hydra-launch-menu
      (:title "Launch Menu" :quit-key "q" :color teal)
      ("Applications"
       (("a" #'org-agenda "Agenda")
	("b" #'calibredb "Calibre")
	("c" #'calendar "Calendar")
	("o" #'ebdb "Contacts")
	("e" #'eww "EWW")
	("m" #'notmuch "Mail")
	("n" (lambda () (interactive) (find-file "~/Org/personal.org")) "Notes")
	("t" #'vterm "Terminal"))
       "Utilities"
       (("f" #'dired "Files")
	("i" (lambda () (interactive) (find-file user-init-file)) "init")
	("j" (lambda () (interactive) (let ((projectile-project-root "~/.emacs.d/")) (projectile-find-file))) "configs")
	("l" #'consult-line "Lookup")
	("r" #'ielm "REPL")
	("s" (lambda () (interactive) (switch-to-buffer "*scratch*")) "Scratch")
	("h" #'helpful-at-point "Help")
	("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
	("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
	))))

  (with-eval-after-load 'emacs
;;; Toggle Menu
    (defvar my/hydra-toggle-menu)
    (pretty-hydra-define my/hydra-toggle-menu
      (:title "Toggle Menu" :quit-key "q" :color pink)
      ("Display 1"
       (("d" #'toggle-frame-fullscreen "fullscreen" :toggle (frame-parameter nil 'fullscreen))
	("c" centered-cursor-mode "center cursor" :toggle t)
	("v" visual-fill-column-mode "fill column" :toggle t)
	("h" hl-line-mode "hl-line" :toggle t)
	("w" #'my/distraction-free "writing" :toggle (default-value (bound-and-true-p olivetti-mode)))
	("L" display-line-numbers-mode "line numbers" :toggle t))
       "Display 2"
       (("l" visual-line-mode "visual line" :toggle t)
	("t" #'consult-theme "theme")
	("m" hide-mode-line-mode "mode line" :toggle t)
	("p" variable-pitch-mode "variable pitch" :toggle (default-value (bound-and-true-p buffer-face-mode))))
       "Editing"
       (("i" #'indent-guide-mode "indent guide")
	("a" copilot-mode "copilot" :toggle t)
	("f" flycheck-mode "flycheck" :toggle t))
       )))
  
  (with-eval-after-load 'outline
;;; Outline Menu
    (defvar my/hydra-outline)
    (pretty-hydra-define my/hydra-outline
      (:title "Outline Menu" :color pink :hint nil :quit-key "z")
      ("Hide"
       (("q" #'outline-hide-sublevels "sublevels")
	("t" #'outline-hide-body "body")
	("o" #'outline-hide-other "other")
	("c" #'outline-hide-entry "entry")
	("l" #'outline-hide-leaves "leaves")
	("d" #'outline-hide-subtree "subtree"))
       "Show"
       (("a" #'outline-show-all "all")
	("e" #'outline-show-entry "entry")
	("i" #'outline-show-children "children")
	("k" #'outline-show-branches "branches")
	("s" #'outline-show-subtree "subtree"))
       "Move"
       (("u" #'outline-up-heading "up")
	("n" #'outline-next-visible-heading "next")
	("p" #'outline-previous-visible-heading "previous")
	("f" #'outline-forward-same-level "forward")
	("b" #'outline-backward-same-level "backward")
	("/" #'consult-outline "outline"))
       "Edit"
       (("k" #'outline-headers-as-kill "kill")
	("U" #'outline-move-subtree-up "move up")
	("D" #'outline-move-subtree-down "move down")
	("<" #'outline-promote "promote")
	(">" #'outline-demote "demote")))))

  (with-eval-after-load 'org
;;; Org Refile
    (defvar my/hydra-org-refile)
    (pretty-hydra-define my/hydra-org-refile
      (:title "Refile" :color teal :quit-key "q")
      ("Refile"
       (("r" (my/refile "resources.org") "Resources")
	("s" (my/refile "personal.org" "Someday/Maybe") "Maybe")
	("t" (my/refile "personal.org" "Tasks") "Tasks"))))
    (bind-key "<f6>" #'my/hydra-org-refile/body 'org-mode-map))

  (with-eval-after-load 'consult-bibtex
    (defvar my/hydra-bibtex)
    (pretty-hydra-define my/hydra-bibtex
      (:title "Bibtex" :color teal :quit-key "q")
      ("Open"
       (("a" consult-bibtex "Search")
	("p" consult-bibtex-open-pdf "Open PDF")
	("o" consult-bibtex-open-any "Open Any")
	("u" consult-bibtex-open-url-or-doi "Open URL/DOI")
	("s" consult-bibtex-show-entry "Show Entry"))
       "Insert"
       (("i" consult-bibtex-insert-key "Insert Key")
	("b" consult-bibtex-insert-bibtex "Insert Bibtex")
	("c" consult-bibtex-insert-citation "Insert Citation")
	("r" consult-bibtex-insert-reference "Insert Reference"))
       "Edit"
       (("n" consult-bibtex-edit-notes "Notes")
	("P" consult-bibtex-add-PDF-attachment "Add PDF Attachment")
	("L" consult-bibtex-add-pdf-to-library "Add PDF to Library"))))
    ;; (bind-key "<f7>" #'my/hydra-bibtex/body 'org-mode-map)
    ))

(provide 'setup-hydra)
;;; setup-hydra.el ends here
