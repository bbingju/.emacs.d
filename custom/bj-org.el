;;; bj-orgmode.el

(use-package org
  :ensure t

  :init (setq org-directory "~/org"
	      org-agenda-files '("~/org")
              org-default-notes-file (concat org-directory "/notes.org")
              org-export-coding-system 'utf-8)

  :mode (("\\.org\\'" . org-mode))

  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))

  :config
  (set-register ?l `(cons 'file ,(concat org-directory "/links.org")))
  ;; refer to http://orgmode.org/manual/Template-elements.html#Template-elements
  (setq org-capture-templates
        '(("l"                          ; hotkey
           "Link"                       ; name
           entry                        ; type
           (file+headline org-default-notes-file "Links") 
           "* %? %^L %^g \n%T" :prepend t)
          ("t" "To Do Item" entry (file+headline org-default-notes-file "To Do Items") 
           "* %?\n%T" :prepend t)
          ("r" "To Read Item" entry (file+headline org-default-notes-file "To Read Items") 
           "* %?\n%T" :prepend t)))

  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)
                                                           (emacs-lisp . t)))

  (setq org-feed-alist
        '(("Slashdot"
           "http://rss.slashdot.org/Slashdot/slashdot"
           "~/org/feeds.org" "Slashdot Entries")))

  ;; Calendar setting
  (require 'calendar)		      ; it's built-in.
  (calendar-set-date-style 'iso)      ; set the "year/month/day" style
  )

(use-package org-bullets
  :requires org
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; org-present: simple presentation plug-in
(use-package org-present
  :requires org
  :ensure t
  :hook ((org-present-mode . (lambda ()
				    (org-present-big)
				    (org-display-inline-images)))
	 (org-present-mode-quit . (lambda ()
				    (org-present-small)
				    (org-remove-inline-images)))))

(provide 'bj-org)
