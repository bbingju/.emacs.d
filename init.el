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
 (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
 (package-initialize))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(defun need-package (name &optional min-version)
  "If feature NAME is not installed with MIN-VERSION optionally,
install it from the ELPA."
  (interactive)
  (when-version-24
   (unless (package-installed-p name min-version)
     (package-install name))))

(need-package 'use-package)
;; (need-package 'editorconfig) ;; https://github.com/editorconfig/editorconfig-emacs#readme
(need-package 'flycheck)
(need-package 'paredit)
(need-package 'python-mode)
(need-package 'auto-complete)
(need-package 'helm-gtags)
(need-package 'solarized-theme)
(need-package 'org2blog)

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
         ("C-x b" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)))

(use-package helm-projectile
  :ensure t
  :config (progn (projectile-global-mode)
                 (setq projectile-completion-system 'helm)
                 (helm-projectile-on)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-swoop
  :ensure t
  :bind (("C-S-s" . helm-swoop)
         ("M-i" . helm-swoop)
         ("M-s s" . helm-swoop)
         ("M-s M-s" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config (progn
            (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
            (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

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
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-suggested-key-mapping t)
 '(make-backup-files nil)
 '(markdown-command "/usr/local/bin/multimarkdown")
 '(markdown-open-command "/usr/local/bin/mark")
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(user-mail-address "pjhwang@gmail.com"))
