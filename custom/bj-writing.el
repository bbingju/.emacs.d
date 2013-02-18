;;; bj-writing.el

;; -----------------------------------------------------------------------------
;; graphviz dot-mode
;; -----------------------------------------------------------------------------
;; (require 'dot-mode)
;; (add-hook 'find-file-hooks 'dot-mode-on)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "graphviz-dot Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))

(custom-set-variables '(markdown-command "markdown_py"))

;; deft - http://jblevins.org/projects/deft/
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/Dropbox/myorg/deft/"
   deft-text-mode 'org-mode)
  (global-set-key (kbd "<f9>") 'deft))

(provide 'bj-writing)
