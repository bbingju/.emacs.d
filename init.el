;;; init.el

(setq custom-dir (expand-file-name "custom" user-emacs-directory))
(setq plugins-dir (expand-file-name "plugins" user-emacs-directory))


(add-to-list 'load-path custom-dir)
(add-to-list 'load-path plugins-dir)

(defmacro when-linux (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
		(cons 'progn body)))

(defmacro when-windows (&rest body)
  (list 'if (string-match "windows" (prin1-to-string system-type))
		(cons 'progn body)))

(defmacro when-version-24 (&rest body)
  (list 'if (>= emacs-major-version 24)
		(cons 'progn body)))

;; Package (ELPA)
(when-version-24
 (require 'package)
 (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
 (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
 (package-initialize))

(require 'bj-hangul)
(require 'bj-ui)
(require 'bj-common)
(require 'bj-programming)
(require 'bj-orgmode)
(require 'bj-writing)

(setq gnus-select-method '(nntp "news.gmane.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(iswitchb-mode t)
 '(column-number-mode t)
 '(transient-mark-mode t)
 '(show-paren-mode t)
 '(make-backup-files nil)
 '(global-auto-revert-mode 1)
 '(global-hl-line-mode 1)
 '(truncate-lines t)			; disable line wrapping
 '(user-mail-address "pjhwang@gmail.com")
 '(markdown-command "markdown_py")
 )

;; Load 'todo.org' file at the starting
(find-file (concat org-directory "/todo.org"))
