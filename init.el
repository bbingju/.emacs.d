;;; init.el --- 
;;; Commentary:

;;; Code:
(setq custom-dir (expand-file-name "custom" user-emacs-directory))
(setq plugins-dir (expand-file-name "plugins" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(defmacro when-mac (&rest body)
  (list 'if (string-match "darwin" (prin1-to-string system-type))
        (cons 'progn body)))

(defmacro when-version-24 (&rest body)
  (list 'if (>= emacs-major-version 24)
		(cons 'progn body)))

(when (file-exists-p custom-file)
  (load custom-file))

;; Package (ELPA)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  ;; https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name)))))

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
         ;; ("C-i" . helm-execute-persistent-action)
         ;; ("C-z" . helm-select-action)
         ))

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

(mapc (lambda (library)
	(require library))
      '(bj-hangul bj-ui bj-common bj-programming bj-org bj-writing))

(use-package bash-completion
  :ensure t
  :config (bash-completion-setup))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(use-package google-this
  :ensure t)

(global-set-key (kbd "ESC <up>") 'enlarge-window)
(global-set-key (kbd "ESC <down>") 'shrink-window)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)
