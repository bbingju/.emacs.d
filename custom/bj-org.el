;;; bj-orgmode.el

(use-package org
  :ensure t
  :init (setq org-directory "~/Dropbox/org"
	      org-agenda-files '("~/Dropbox/org"))
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda)))

(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)
(setq org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"solarized-light.css\" />")

(setq org-default-notes-file (concat org-directory "/notes.org"))

(set-register ?l (cons 'file "~/Dropbox/org/links.org"))

;; refer to http://orgmode.org/manual/Template-elements.html#Template-elements
(setq org-capture-templates
      '(("l" "Link" entry (file+headline "~/Dropbox/org/links.org" "Links") 
         "* %? %^L %^g \n%T" :prepend t)
        ("t" "To Do Item" entry (file+headline "~/Dropbox/org/i.org" "To Do Items") 
         "* %?\n%T" :prepend t)
        ("r" "To Read Item" entry (file+headline "~/Dropbox/org/toread.org" "To Read Items") 
         "* %?\n%T" :prepend t)))

(org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)
                                                         (emacs-lisp . t)))

;; org-present: simple presentation plug-in
(use-package org-present
  :ensure t
  :hook ((org-present-mode . (lambda ()
				    (org-present-big)
				    (org-display-inline-images)))
	 (org-present-mode-quit . (lambda ()
				    (org-present-small)
				    (org-remove-inline-images)))))

;; Calendar setting
(require 'calendar)		      ; it's built-in.
(calendar-set-date-style 'iso)	      ; set the "year/month/day" style

(provide 'bj-org)
