;;; For Python programming
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-jedi
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-jedi))

(use-package python-mode
  :ensure t)

(use-package elpy
  :ensure t
  :config (elpy-enable))

(use-package flycheck-pyflakes
  :ensure t
  :defer 5
  :config (add-hook 'python-mode-hook 'flycheck-mode))


(provide 'lang-python)
