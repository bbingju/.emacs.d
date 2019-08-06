;;; bj-appearance.el --- 
;;; Commentary:

;;; Code:

(defun bj-appearance-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(when window-system
 (global-set-key [f11] 'bj-appearance-fullscreen))


(setq inhibit-splash-screen t)

(when-mac
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  ;; (setq ns-use-proxy-icon  nil)
  ;; (setq frame-title-format nil))

;;; Turn off early to avoid momentary display.
(mapc
 (lambda (mode)
   (if (fboundp mode)
       (funcall mode -1)))
 '(tool-bar-mode scroll-bar-mode))

;;; Color Theme
;;; ex) material, atom-dark, solarized-dark, zenburn, etc.
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-vibrant t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;; https://github.com/seagle0128/doom-modeline
;;; This package requires the fonts included with 'all-the-icons' to be installed.
;;; Run 'M-x all-the-icons-install-fonts' to do so.
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

(set-default 'cursor-type 'box)

;; face setting
(when (window-system)
  (require 'fontutil)
  ;; (set-face-attribute 'default nil :family "Monospace") ; Monospace, Consolas, Monaco, Liberation Mono, Hack, etc.
  ;; (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  ;; (set-face-attribute 'default nil :height 140)

  (when-linux (fontutil/set-font "ubuntu-14")
              (setq-default line-spacing 4))

  (when-mac (fontutil/set-font "firacode-14")
  	    (setq-default line-spacing 4))

  (when-windows (fontutil/set-font "d2coding-14")
        	(setq-default line-spacing 4))
  )

(provide 'bj-appearance)
