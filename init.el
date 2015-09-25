;;; init.el --- 
;;; Commentary:

;;; Code:

(setq custom-dir (expand-file-name "custom" user-emacs-directory))
(setq plugins-dir (expand-file-name "plugins" user-emacs-directory))


(add-to-list 'load-path custom-dir)
(add-to-list 'load-path plugins-dir)

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-windows ()
  (interactive)
  (string-equal system-type "windows-nt"))

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

(defun need-package (name &optional min-version)
  "If feature NAME is not installed with MIN-VERSION optionally,
install it from the ELPA."
  (interactive)
  (when-version-24
   (unless (package-installed-p name min-version)
     (package-install name))))


(need-package 'magit)
(need-package 'editorconfig) ;; https://github.com/editorconfig/editorconfig-emacs#readme
(need-package 'helm)
(need-package 'flycheck)
(need-package 'paredit)
(need-package 'python-mode)
(need-package 'auto-complete)
(need-package 'yasnippet)
(need-package 'solarized-theme)
(need-package 'markdown-mode)
(need-package 'org2blog)

(require 'bj-hangul)
(require 'bj-ui)
(require 'bj-common)
(require 'bj-programming)
(require 'bj-orgmode)
(require 'bj-writing)
(require 'helm-config)

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
 '(column-number-mode t)
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(global-auto-revert-mode 1)
 '(global-hl-line-mode 1)
 '(make-backup-files nil)
 '(markdown-command "/usr/local/bin/multimarkdown")
 '(markdown-open-command "/usr/local/bin/mark")
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(user-mail-address "pjhwang@gmail.com"))
