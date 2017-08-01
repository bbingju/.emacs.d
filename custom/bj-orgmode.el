;;; bj-orgmode.el

(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)
(setq org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"solarized-light.css\" />")

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-directory))
(setq org-mobile-files '("~/Dropbox/org/todo.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-agenda-files '("~/Dropbox/org/todo.org"
				"~/Dropbox/org/study.org"
				"~/Dropbox/org/travels.org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg-new")

(setq org-default-notes-file (concat org-directory "/notes.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitch)

;; org-present: simple presentation plug-in
;; (add-to-list 'load-path "~/path/to/org-present")
(autoload 'org-present "org-present" nil t)

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))


;; Calendar setting
(require 'calendar)		      ; it's built-in.
(calendar-set-date-style 'iso)	      ; set the "year/month/day" style

(provide 'bj-orgmode)
