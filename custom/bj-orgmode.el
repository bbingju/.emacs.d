;;; bj-orgmode.el

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/myorg")
(setq org-agenda-files '("~/Dropbox/myorg"))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-directory))
(setq org-mobile-files '("~/Dropbox/myorg/todo.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-agenda-files '("~/Dropbox/myorg/todo.org"
				"~/Dropbox/myorg/study.org"
				"~/Dropbox/myorg/travels.org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg-new")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Calendar setting
(require 'calendar)		      ; it's built-in.
(calendar-set-date-style 'iso)	      ; set the "year/month/day" style

(provide 'bj-orgmode)
