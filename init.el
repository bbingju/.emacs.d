;;; init.el

(setq custom-dir (expand-file-name "custom" user-emacs-directory))

(add-to-list 'load-path custom-dir)
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))


;; Package (ELPA)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

(require 'bj-hangul)
(require 'bj-ui)
(require 'bj-common)
(require 'bj-programming)
(require 'bj-orgmode)
(require 'bj-writing)

(require 'htmlize)

;;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/mysnippets"
			 "~/.emacs.d/elpa/yasnippet-0.8.0/snippets"))
(yas-global-mode 1)


;; key mapping
(global-set-key [f12] 'toggle-truncate-lines) ; F12 to toggle line wrap

(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.
(global-set-key "\M-]" 'find-next-tag)
(global-set-key "\M-[" 'find-prev-tag)
(global-set-key [C-return] 'semantic-complete-analyze-inline)


(setq default-frame-alist
	  '(
	(top . 0)
	(left . 0)
	(width . 100)
	(height . 50)
	))

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
 )

;; Load 'todo.org' file at the starting
(find-file "~/Dropbox/myorg/todo.org")
