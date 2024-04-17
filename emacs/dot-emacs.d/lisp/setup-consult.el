;;; setup-consult --- Configuration for consult
;;; Commentary:
;;; Code:

(use-package consult
  :straight t
  :commands (consult-customize)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command)
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-x t b" . consult-buffer-other-tab)
  ("C-x r b" . consult-bookmark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-o" . consult-outline)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-fd)
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
	("M-e" . consult-isearch-history)
	("M-s e" . consult-isearch-history)
	("M-s l" . consult-line)
	("M-s L" . consult-line-multi))
  (:map minibuffer-local-map
	("M-s" . consult-history)
	("M-r" . consult-history)))

(use-package consult-org
  :commands (consult-org-heading)
  :after consult
  :config
  (bind-key "C-c h" #'consult-org-heading 'org-mode-map))

(use-package consult-flycheck
  :straight t
  :after consult)

(use-package consult-recoll
  :straight t
  :commands (consult-recoll consult-recoll-embark-setup)
  :after consult
  :config
  (consult-recoll-embark-setup))

(use-package consult-notes
  :straight t
  :commands (consult-notes
	     consult-notes-denote-mode
	     consult-notes-denote--state
	     consult-notes-denote--new-note)
  :custom-face
  (consult-notes-sep ((t (:foreground "CornFlowerBlue"))))
  :bind
  ("C-c n o" . consult-notes)
  ("C-c n X" . consult-notes-search-in-all-notes)
  ("C-c n 4 o" . my/consult-notes-other-window)
  :custom
  (consult-notes-file-dir-sources '(("Org" ?o "~/Dropbox/Org/")))
  (consult-notes-org-headings-files '("~/Dropbox/Org/"))
  ;; search only for text files in denote dir
  :config
  (when (locate-library "denote")
    (setopt consult-notes-denote-display-id nil)
    (setopt consult-notes-denote-files-function 'denote-directory-files)
    (setopt consult-notes-denote-truncate-title 60)
    (consult-notes-denote-mode))

  ;; (consult-notes-org-headings-mode)

  (defun my/consult-notes-other-window ()
    "Open a note in another window"
    (interactive)
    (let ((consult--buffer-display #'switch-to-buffer-other-window))
      (consult-notes)))

  ;; NOTE: ' and #' don't seem to work here.
  (consult-customize consult-notes my/consult-notes-other-window :preview-key "M-."))

;;; consult-notes-denote padding
(with-eval-after-load 'consult-notes-denote
  (defvar consult-notes-denote-truncate-title nil
    "Truncate title in Denote notes. Can be nil or a number.")
  (declare-function denote-filetype-heuristics "denote" (FILE))
  (declare-function denote-retrieve-title-value "denote" (ARG1 ARG2))
  (declare-function denote-retrieve-filename-title "denote" (FILE))
  (declare-function denote-retrieve-filename-identifier "denote" (FILE))
  (declare-function denote-extract-keywords-from-path "denote" (FILE))

  (defconst consult-notes-denote--source
    (list :name     (propertize "Denote notes" 'face 'consult-notes-sep)
	  :narrow   ?d
	  :category (bound-and-true-p consult-notes-category)
	  :annotate (bound-and-true-p consult-notes-denote-annotate-function)
	  :items    (lambda ()
		      (let* ((max-width (if consult-notes-denote-truncate-title consult-notes-denote-truncate-title 0))
			     (cands (mapcar (lambda (f)
					      (let* ((id (denote-retrieve-filename-identifier f))
						     (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f)) (denote-retrieve-filename-title f)))
						     (title (if (bound-and-true-p consult-notes-denote-display-id)
								(concat id " " title-1)
							      title-1))
						     (title (if consult-notes-denote-truncate-title
								(truncate-string-to-width title consult-notes-denote-truncate-title) title))
						     (dir (file-relative-name (file-name-directory f) (bound-and-true-p denote-directory)))
						     (keywords (denote-extract-keywords-from-path f)))
						(if (not consult-notes-denote-truncate-title)
						    (let ((current-width (string-width title)))
						      (when (> current-width max-width)
							(setq max-width current-width))))
						(propertize title 'denote-path f 'denote-keywords keywords)))
					    (funcall (bound-and-true-p consult-notes-denote-files-function)))))
			(mapcar (lambda (c)
				  (let* ((keywords (get-text-property 0 'denote-keywords c))
					 (path (get-text-property 0 'denote-path c))
					 (dirs (directory-file-name (file-relative-name (file-name-directory path) (bound-and-true-p denote-directory)))))
				    (concat c
					    ;; align keywords
					    (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
					    (format "%18s"
						    (if keywords
							(concat (propertize "#" 'face 'consult-notes-name)
								(propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
						      ""))
					    (when (bound-and-true-p consult-notes-denote-dir) (format "%18s" (propertize (concat "/" dirs) 'face 'consult-notes-name))))))
				cands)))
	  ;; Custom preview
	  :state  #'consult-notes-denote--state
	  ;; Create new note on match fail
	  :new     #'consult-notes-denote--new-note)))

(defun consult-notes-my-embark-function (cand)
  "Do something with CAND."
  (interactive "fNote: ")
  (message cand))

(use-package consult-flyspell
  :straight t
  :after consult
  :commands (consult-flyspell)
  :config
  (with-eval-after-load 'flyspell
    (bind-key "C-;" #'consult-flyspell 'flyspell-mode-map))
  :custom
  (consult-flyspell-select-function 'flyspell-correct-at-point)
  (consult-flyspell-set-point-after-word t)
  (consult-flyspell-always-check-buffer nil))

(use-package consult-bibtex
  :after consult
  :straight '(consult-bibtex :host github :repo "mohkale/consult-bibtex")
  :config
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map))))


(provide 'setup-consult)
;;; setup-consult.el ends here
