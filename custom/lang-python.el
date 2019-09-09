;;; For Python programming
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  (elpy-enable))

(use-package elpy
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)))

(use-package pip-requirements
  :ensure t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8
  :ensure t)

(use-package company-jedi
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-jedi))

(use-package flycheck-pyflakes
  :ensure t
  :defer 5
  :hook (python-mode . flycheck-mode))

(provide 'lang-python)
