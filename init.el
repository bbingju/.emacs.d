;;; init.el --- 
;;; Commentary:

;;; Code:
(setq user-full-name "Byung Ju Hwang"
      user-mail-address "pjhwang@gmail.com")

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
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
 (package-initialize))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'bj-hangul)
(require 'bj-ui)
(require 'bj-common)
(require 'bj-programming)
(require 'bj-orgmode)
(require 'bj-writing)

;;; For Helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-yas-display-key-on-candidate t
          helm-M-x-requires-pattern nil
          helm-split-window-in-side-p t ; open helm buffer inside
                                        ; current window, not occupy
                                        ; whole other window
          helm-move-to-line-cycle-in-source  t ; move to end or
                                        ; beginning of source
                                        ; when reaching top or
                                        ; bottom of source.
          helm-ff-search-library-in-sexp t ; search for library in
                                        ; `require' and
                                        ; `declare-function' sexp.
          helm-scroll-amount 8 ; scroll 8 lines other window using
                                        ; M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)
    (helm-mode))
  :bind (("C-c h" . helm-command-prefix)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)))

(use-package helm-gtags
  :ensure t
  :bind (:map helm-gtags-mode-map
              ("M-t" . helm-gtags-find-tag)
              ("M-r" . helm-gtags-find-rtag)
              ("M-s" . helm-gtags-find-symbol)
              ("M-g M-p" . helm-gtags-parse-file)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history)
              ("M-," . helm-gtags-pop-stack)))

(use-package helm-projectile
  :ensure t
  :config (progn (projectile-global-mode)
                 (setq projectile-completion-system 'helm)
                 (helm-projectile-on)))

(use-package helm-descbinds
  :ensure t
  :defer 7
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-swoop
  :ensure t
  :bind (("C-S-s" . helm-swoop)
	 ("M-i" . helm-swoop)
	 ("M-s s" . helm-swoop)
	 ("M-s M-s" . helm-swoop)
	 ("M-I" . helm-swoop-back-to-last-point)
	 ("C-c M-i" . helm-multi-swoop)
	 ("C-x M-i" . helm-multi-swoop-all)
	 ("M-i" . helm-multi-swoop-all-from-helm-swoop)
	 :map isearch-mode-map
	 ("M-i" . helm-swoop-from-isearch)))

(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))


(global-set-key (kbd "ESC <up>") 'enlarge-window)
(global-set-key (kbd "ESC <down>") 'shrink-window)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

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
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-gtags-suggested-key-mapping t)
 '(make-backup-files nil)
 '(markdown-command "/usr/local/bin/multimarkdown")
 '(markdown-open-command "/usr/local/bin/mark")
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(user-mail-address "pjhwang@gmail.com"))
