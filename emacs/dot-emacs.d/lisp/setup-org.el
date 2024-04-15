;; -*- lexical-binding: t; -*-
;; * ORG

(use-package org
  :straight nil
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . (lambda nil (setq cursor-type 'bar)))
  (org-capture-mode . (lambda nil (setq-local
				   header-line-format
				   (substitute-command-keys
				    "\\<org-capture-mode-map>Capture buffer.  Finish \
`\\[my/org-capture-finalize]', refile `\\[org-capture-refile]', \
abort `\\[org-capture-kill]'."))))
  :bind
  ("C-x c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("C-c 4 o" . my/org-open-at-point-other-window)
        ("C-c 4 C-o" . my/org-open-at-point-other-window)
        ("C-c 5 C-o" . my/org-open-at-point-other-frame)
        ("C-c 5 o" . my/org-open-at-point-other-frame)
        ("C-c e" . org-emphasize)
	("<C-i>" . org-delete-backward-char)
	("M-i" . backward-kill-word)
	("C-c <C-i>" . org-cycle)
	("C-c C-x h" . org-edit-headline)
	("C-c C-x <DEL>" . org-cut-subtree)
	("C-c C-x C-<backspace>" . org-cut-subtree))
  (:map org-capture-mode-map
	("C-c C-c" . my/org-capture-finalize))

  :custom-face
  (org-level-1 ((t (:bold nil))))
  (org-level-2 ((t (:bold nil))))
  (org-level-3 ((t (:bold nil))))
  (org-level-4 ((t (:bold nil))))
  (org-level-5 ((t (:bold nil))))
  (org-level-6 ((t (:bold nil))))
  (org-level-7 ((t (:bold nil))))
  (org-level-8 ((t (:bold nil))))
  (org-document-title ((t (:height 1.7))))
  (org-table ((t (:inherit 'fixed-pitch))))
  (org-block ((t (:inherit 'fixed-pitch))))
  (org-block-begin-line ((t (:inherit 'fixed-pitch))))
  (org-document-info-keyword ((t (:inherit 'fixed-pitch))))
  (org-meta-line ((t (:inherit 'fixed-pitch))))
  (org-document-info ((t (:inherit 'fixed-pitch))))
  (org-property-value ((t (:inherit 'fixed-pitch))))
  (org-drawer ((t (:inherit 'fixed-pitch))))
  (org-special-keyword ((t (:inherit 'fixed-pitch))))
  (org-column-title ((t (:inherit 'fixed-pitch))))
  (org-agenda-structure ((t (:height 1.5))))
  (org-agenda-date ((t (:height 1.2))))
  (org-archived ((t (:background unspecified :foreground "grey"))))
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
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE")))
  (setq org-hide-emphasis-markers t)
  (setq org-latex-compiler "xelatex")

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 2)
	  ("refile.org" :level . 0)
	  ("resources.org" :level . 0)))
  (setq org-ellipsis "↴")
  (setq org-src-preserve-indentation t)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-fast-tag-selection-single-key t)
  (setq org-special-ctrl-a/e t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-goto-max-level 5)
  ;;(setq org-highlight-latex-and-related '(latex script entities))
  (setq org-highlight-latex-and-related nil)
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (setq org-src-window-setup 'current-window)
  (add-to-list 'org-babel-load-languages '(shell . t))
  (setq org-tags-column 0)
  (setq org-auto-align-tags nil)
  (setq org-tags-exclude-from-inheritance '("ARCHIVE" "project"))
  (setq org-global-properties '(("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00")))
  (setq org-refile-use-outline-path 'file)
  (setq org-log-into-drawer t)
  (setq org-habit-graph-column 55)
  (setq org-attach-use-inheritance t)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; ** org-agenda
  
  (setq org-agenda-files '("daily.org" "personal.org" "phone.org" "read.org"))
  ;; after https://emacs.stackexchange.com/questions/75822/ignoring-non-existent-org-mode-agenda-files
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-custom-commands 	; a,e,t,m,s,T,M,S,C
	'(("p"  . "project+state search")
	  ("pp" tags "+project")
	  ("pa" tags "+project-TODO=\"MAYBE\"")
	  ("pm" tags "+project+TODO=\"MAYBE\"")
	  ("o" . "todo related")
	  ("os" tags "TODO=\"TODO\"" ((org-agenda-skip-function
				       '(org-agenda-skip-entry-if 'timestamp))))
	  ("ot" todo "TODO" nil)
	  ("op" todo "PROG" nil)
	  ("op" todo "NEXT" nil)
	  ("om" todo "MAYBE" nil)
	  ("n" "Agenda / INTR / PROG / NEXT"
	   ((agenda "" nil)
	    (todo "INTR" nil)
	    (todo "PROG" nil)
	    (todo "NEXT" nil))
	   nil)))
  (setq org-agenda-include-diary t)
  
  ;; ** org-capture

  (defun my/read-later-template (url)
    "capture template for read later"
    (let* ((article (my/read-it-later-attach url))
	   (name (nth 0 article))
	   (file (nth 1 article))
	   (dir (nth 2 article))
	   (url (nth 3 article))
	   (effort (org-minutes-to-clocksum-string
		    ( / (string-to-number
			 (string-trim (shell-command-to-string
				       (concat "wc -w < '" (expand-file-name file dir)"'")))) 100))))
      (concat "* TODO " name "\n:PROPERTIES:\n:URL: " url "\n:Effort: " effort "\n:END:\n%U\nAvailable at: [[attachment:" file "][" name "]]\n%?")))

  (defun my/read-later-template-from-kill ()
    (require 'org-web-tools)
    (my/read-later-template (org-web-tools--get-first-url)))

  (defun my/read-later-template-from-prompt ()
    (my/read-later-template (read-string "URL: ")))

  (defun my/url-librewolf-capture-to-org ()
    "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
    (interactive)
    (org-capture-string (my/org-mac-link-librewolf-get-frontmost-url) "lz")
    (ignore-errors)
    (org-capture-finalize)
    nil)
  
  (setq org-capture-templates
        '(("r" "refile" entry (file "refile.org")
	   "* %^{Title}\n%U\n\n%?" :prepend t :empty-lines-after 1)
	  ;; ("t" "today" entry (file+olp+datetree "daily.org")
	  ;;  "* %^{Title}\n\n%?")
	  ;; ("T" "today+open" entry (file+olp+datetree "daily.org")
	  ;;  "* %^{Title}\n\n%?" :jump-to-captured t)
	  ("j" "Journal" entry (file+olp+datetree "journal.org")
	   "* %U %^{Title}\n%i\n\n%?")
	  ("p" "project")
	  ("pn" "project simple" entry (id "316F33BA-71DE-41B9-B21B-928D3778A097")
	   "* TODO %^{Title} %^{CATEGORY}p [/] :project:\n- [ ] %?" :prepend t)
	  ("pN" "project elaborate" entry (id "316F33BA-71DE-41B9-B21B-928D3778A097")
	   (file "~/.emacs.d/capture/project.org") :prepend t)
	  ("c" "clock")
	  ("ct" "clock task" entry (clock) (file "~/.emacs.d/capture/task.org") :prepend t)
	  ("cp" "clock project" entry (clock) (file "~/.emacs.d/capture/project.org")
	   "* TODO %^{Title} [/] :project:\n- [ ] %?" :prepend t)
	  ("t" "Task" entry (id "316F33BA-71DE-41B9-B21B-928D3778A097")
	   (file "~/.emacs.d/capture/task.org") :prepend t)
	  ("h" "Habit" entry (id "316F33BA-71DE-41B9-B21B-928D3778A097")
	   (file "~/.emacs.d/capture/habit.org") :prepend t)
	  ("l" "later")
	  ("lp" "Read later prompt" entry (id "F86FBB48-767F-436D-926E-D118F57AE534")
	   (function my/read-later-template-from-prompt))
	  ("lk" "Read later kill" entry (id "F86FBB48-767F-436D-926E-D118F57AE534")
	   (function my/read-later-template-from-kill))
	  ("ll" "Read later librewolf" entry (file "refile.org")
	   "* %(my/org-mac-link-librewolf-get-frontmost-url) :link:\n%U" :immediate-finish t :prepend t)
	  ("lL" "Read later librewolf edit" entry (file "refile.org")
	   "* %(my/org-mac-link-librewolf-get-frontmost-url) :link:\n%U\n%?" :prepend t)
	  ("w" "Web template" entry (file "refile.org")
           "* %?Capture from web\nSource: %:link,\n\nTitle: %:description\n\n#+begin_quote\n%i\n#+end_quote" :empty-lines 1)
	  ))

  ;; ** org-capture frame

  (defun my/make-capture-frame ()
    "Create a new frame and run `org-capture'."
    (interactive)
    (make-frame '((name . "capture")
                  (top . 300)
                  (left . 700)
                  (width . 80)
                  (height . 25)))
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (condition-case ex
	  (org-capture)
	('error
	 ;;(message "org-capture: %s" (error-message-string ex))
	 (delete-frame)))))

  (defun my/close-if-capture (&optional a)
    (if (equal "capture" (frame-parameter nil 'name))
	(delete-frame))
    (if (eq 'darwin system-type)
	(ns-do-applescript "tell application \"LibreWolf\" to activate")))

  (defun my/org-capture-finalize (&optional stay-with-capture)
    (interactive "P")
    (org-capture-finalize stay-with-capture))

  (advice-add 'my/org-capture-finalize :after #'my/close-if-capture)

  ;;(advice-add 'org-capture-finalize :after #'my/close-if-capture)
  (advice-add 'org-capture-refile :after #'my/close-if-capture)
  (advice-add 'org-capture-kill :after #'my/close-if-capture)
  (advice-add 'my/read-later-template-from-prompt :after #'my/close-if-capture)
  (advice-add 'org-protocol-capture :before
	      (lambda (_) (progn
			    (make-frame '((name . "capture")
					  (top . 300)
					  (left . 700)
					  (width . 80)
					  (height . 25)))
			    (select-frame-by-name "capture")
			    (my/frame-recenter))))
  (advice-add 'org-protocol-capture :after (lambda (_) (delete-other-windows)))

  ;; ** org align tags

  (defun my-org--align-tags-here (to-col)
    "Align tags on the current headline to TO-COL.
Since TO-COL is derived from `org-tags-column', a negative value is
interpreted as alignment flush-right, a positive value as flush-left,
and 0 means insert a single space in between the headline and the tags."
    ;; source: https://list.orgmode.org/[email protected]/
    (save-excursion
      (when (org-match-line org-tag-line-re)
	(let* ((tags-start (match-beginning 1))
	       (tags-end (match-end 1))
	       (tags-pixel-width
		(car (window-text-pixel-size (selected-window)
					     tags-start tags-end)))
	       (blank-start (progn
			      (goto-char tags-start)
			      (skip-chars-backward " \t")
			      (point)))
	       ;; use this to avoid a 0-width space before tags on long lines:
	       (blank-start-col (progn
				  (goto-char blank-start)
				  (current-column)))
	       ;; this is to makes it work with org-indent-mode:
	       (lpref (if (org-fold-folded-p) 0
			(length (get-text-property (point) 'line-prefix)))))
	  ;; If there is more than one space between the headline and
	  ;; tags, delete the extra spaces.  Might be better to make the
	  ;; delete region one space smaller rather than inserting a new
	  ;; space?
	  (when (> tags-start (1+  blank-start))
	    (delete-region blank-start tags-start)
	    (goto-char blank-start)
	    (insert " "))
	  (if (or (= to-col 0) (< (abs to-col) (1- blank-start-col)))
	      ;; Just leave one normal space width
	      (remove-text-properties blank-start (1+  blank-start)
				      '(my-display nil))
	    (message "In here: %s" lpref)
	    (let ((align-expr
		   (if (> to-col 0)
		       ;; Left-align positive values
		       (+ to-col lpref)
		     ;; Right-align negative values by subtracting the
		     ;; width of the tags.  Conveniently, the pixel
		     ;; specification allows us to mix units,
		     ;; subtracting a pixel width from a column number.
		     `(-  ,(- lpref to-col) (,tags-pixel-width)))))
	      (put-text-property blank-start (1+  blank-start)
				 'my-display
				 `(space . (:align-to ,align-expr)))))))))

  (defun my-fix-tag-alignment ()
    (setq org-tags-column 70) ;; adjust this
    (advice-add 'org--align-tags-here :override #'my-org--align-tags-here)
    ;; this is needed to make it work with https://github.com/minad/org-modern:
    (add-to-list 'char-property-alias-alist '(display my-display))
    ;; this is needed to align tags upon opening an org file:
    (org-align-tags t))

  ;;(add-hook 'org-mode-hook #'my-fix-tag-alignment)


  ;; ** org read it later

  (defun my/org-web-tools--url-as-readable-org-file (&optional url)
    "Return string containing Org entry of URL's web page content.
Content is processed with `eww-readable' and Pandoc.  Entry will
be a top-level heading, with article contents below a
second-level \"Article\" heading, and a timestamp in the
first-level entry for writing comments."
    ;; By taking an optional URL, and getting it from the clipboard if
    ;; none is given, this becomes suitable for use in an org-capture
    ;; template, like:

    ;; ("wr" "Capture Web site with eww-readable" entry
    ;;  (file "~/org/articles.org")
    ;;  "%(org-web-tools--url-as-readable-org)")
    (-let* ((url (or url (org-web-tools--get-first-url)))
            (dom (plz 'get url :as #'org-web-tools--sanitized-dom))
            ((title . readable) (org-web-tools--eww-readable dom))
            (title (org-web-tools--cleanup-title (or title "")))
            (converted (org-web-tools--html-to-org-with-pandoc readable))
            (link (org-link-make-string url title))
            (timestamp (format-time-string (org-time-stamp-format 'with-time 'inactive))))
      (with-temp-buffer
	(org-mode)
	;; Insert article text
	(insert converted)
	;; Demote in-article headings
	;; MAYBE: Use `org-paste-subtree' instead of demoting headings ourselves.
	;;(org-web-tools--demote-headings-below 1)
	;; Insert headings at top
	(goto-char (point-min))
	(insert "#+title: " title "\n"
		"#+date: " timestamp "\n\n"
		link "\n\n")
	(buffer-string))))

  (defun my/read-it-later-attach (url)
    "Attach the URL as readable Org to the entry at point."
    (interactive "sURL: ")
    (save-window-excursion
      (org-id-open "F86FBB48-767F-436D-926E-D118F57AE534" nil)
      (let ((attach-dir (org-attach-dir)))
	(with-temp-buffer
	  (message "Getting Article...")
	  (insert (my/org-web-tools--url-as-readable-org-file url))
	  (goto-char (point-min))
	  (let ((name (cadar (org-collect-keywords '("TITLE")))))
	    (message (org-get-title))
	    (write-file (expand-file-name
			 (concat (denote-sluggify-title name) ".org")
			 attach-dir))
	    (org-link--add-to-stored-links (concat "attachment:" (buffer-name)) name)
	    (list name (buffer-name) attach-dir url))))))

  ;; ** org open other window

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

  ;; ** END

  )


;; * ORG EXPORT
;; ** ox-hugo

(use-package ox-hugo
  ;;  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :config
  (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t))))

(with-eval-after-load 'ox-hugo
  ;;;; Export Block
  (defun org-hugo-export-block (export-block _contents _info)
    "Transcode a EXPORT-BLOCK element from Org to Hugo-compatible Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information.

Example:

  #+begin_export hugo
  foo
  #+end_export

exports verbatim to \"foo\" only when exported using `hugo'
backend.

If the backend tag is \"markdown\"/\"md\" or \"html\", exporting
of those blocks falls back to the respective exporters."
    (cond
     ((string= (org-element-property :type export-block) "HUGO")
      (org-remove-indentation (org-element-property :value export-block)))
     ((string= (org-element-property :type export-block) "LATEX")
      (org-export-with-backend 'latex export-block nil nil))
     ;; Also include Markdown and HTML export blocks.
     ;; ox-md handles HTML export blocks too.
     (t
      (org-export-with-backend 'md export-block nil nil)))))

;; ** ox-pandoc

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


;; * ORG BABEL
;; ** ob-python

(use-package ob-python
  :straight nil
  :after org
  :config
  (setq org-babel-python-command "python3"))

;; ** ob-ipython

(use-package ob-ipython
  :disabled
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t))))

;; ** ob-julia

(use-package ob-julia
  :straight nil
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


;; ** ob-shell

(use-package ob-shell
  :straight nil
  :after org
  :config
  (setq org-babel-default-header-args:sh '((:results . "output")))
  (setq org-babel-default-header-args:shell '((:results . "output"))))
;; ** ob-latex

(use-package ob-latex
  :straight nil
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t))))
;; ** ob-async

;; python does not work.
(use-package ob-async
  :disabled
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "ipython"))
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
		(setq inferior-julia-program-name "/usr/local/bin/julia"))))
;; ** mixed-pitch-mode

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; * PLUGINS 
;; ** org-protocol

(use-package org-protocol
  :straight nil
  :after org
  :custom
  (org-protocol-protocol-alist
   '(("eww" :protocol "eww" :function my/open-in-eww-protocol)
     ("body" :protocol "body" :function my/org-protcol-capture-body)))
  :config
  (defun my/open-in-eww-protocol (info)
    (x-focus-frame nil)
    (let ((url (plist-get info :url)))
      (eww url)))

  (defun my/org-protcol-capture-body (info)
    (let ((body (plist-get info :body)))
      (message "have body")
      (eww-open-file (make-temp-file "org-protocol" nil ".html" body)))))

;; ** org-contrib

(use-package org-contrib)
;; ** org-super-agenda

(use-package org-super-agenda
  :after org-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today"  ; Optionally specify section name
            :time-grid t  ; Items that appear on the time grid
            :todo "TODAY")  ; Items that have this TODO keyword
     (:name "Important"
            ;; Single arguments given alone
            :tag "bills"
            :priority "A")
     (:priority<= "B"
                  ;; Show this section after "Today" and "Important", because
                  ;; their order is unspecified, defaulting to 0. Sections
                  ;; are displayed lowest-number-first.
                  :order 1)
     (:name "Habits"
	    :habit t
	    :order 2)))
  :custom-face
  (org-super-agenda-header ((t (:height 0.75))))
  :bind
  (:map org-agenda-mode-map
	("C-c C-x C-a" . org-agenda-archive-default)))

;; ** org-appear

(use-package org-appear
  ;;:hook org-mode
  :custom
  (org-appear-autolinks t))

;; ** org-fragtog

(use-package org-fragtog
  :hook org-mode)

;; ** TODO org-ref

(use-package org-ref
  :after org
  :init
  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format.

Do this only if the export backend is `html' or a derivative of
that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html)))
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html)))

;; ** org-remark

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

;; ** org-mac-link

(use-package org-mac-link
  :when (eq system-type 'darwin)
  :commands (my/org-mac-link-librewolf-get-frontmost-url)
  :after org
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
	("C-c L" . my/org-mac-link-get-link))
  :config
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
	    descriptors))))

;; ** org-noter

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

;; ** org-mind-map

(use-package org-mind-map
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )
;; ** org-ql

(use-package org-ql)

;; ** org-web-tools

(use-package org-web-tools)

;; * LOCAL-VARIABLES

(provide 'setup-org)

;; Local Variables:
;; outline-regexp: " *;; \\*+"
;; page-delimiter: " *;; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-body)
;; coding: utf-8-unix
;; End:
