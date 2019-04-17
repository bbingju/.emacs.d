(require 'use-package)

(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
  :bind (:map emacs-lisp-mode-map
              ("<f6>" . eval-buffer)
              ("M-<f6>" . emacs-lisp-byte-compile-and-load)
              ("<return>" . newline-and-indent)))

(use-package ielm
  :commands ielm)

(use-package eldoc-mode
  :hook (emacs-lisp-mode ielm-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode ielm-mode) . rainbow-delimiters-mode))

(provide 'lang-elisp)
