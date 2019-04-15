;;; For Python programming
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode))

(use-package company-jedi
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-jedi))


(use-package elpy
  :ensure t
  :config (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i"))

(use-package flycheck-pyflakes
  :ensure t
  :defer 5
  :hook (python-mode . flycheck-mode))

(provide 'lang-python)
