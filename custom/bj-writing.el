;;; bj-writing.el

;; -----------------------------------------------------------------------------
;; graphviz dot-mode
;; -----------------------------------------------------------------------------
(use-package graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :init
  (autoload 'graphviz-dot-mode "graphviz-dot-mode" "graphviz-dot Editing Mode" t))

(use-package gnuplot-mode
  :mode ("\\.plt\\'" . gnuplot-mode))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t))

;; deft - http://jblevins.org/projects/deft/
(use-package deft
  :ensure t
  :bind ("<f9>" . deft)
  :config (setq deft-extensions '("org" "md" "txt")
                deft-directory "~/Dropbox/wiki" ;(concat org-directory "/deft/")
                deft-auto-save-interval 0
                deft-text-mode 'org-mode))

(provide 'bj-writing)
