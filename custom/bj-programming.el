;;; bj-programming.el ---
;;; Commentary:

;;; Code:

;;; Enable line numbers when programming mode
(use-package display-line-numbers-mode
  :hook prog-mode)

;; yasnippet
(use-package yasnippet
  :defer 5
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; editorconfig
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; (use-package ggtags
;;   :ensure t
;;   :init (add-hook 'c-mode-common-hook
;;                   (lambda ()
;;                     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                       (ggtags-mode 1)))))

(use-package company
  :ensure t
  :bind ("C-M-i" . company-complete)
  :init (add-hook 'after-init-hook `global-company-mode))

(use-package company-c-headers
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-c-headers))

(use-package company-shell
  :ensure t
  :after company
  :defer 5
  :config (add-to-list 'company-backends 'company-shell))

(use-package rainbow-delimiters
  :ensure t)

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'helm)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package helm-gtags
  :ensure t
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-find-tag)
              ("M-r" . helm-gtags-find-rtag)
              ("M-s" . helm-gtags-find-symbol)
              ("M-g M-p" . helm-gtags-parse-file)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history)
              ("M-," . helm-gtags-pop-stack))
  :config (setq helm-gtags-path-style 'relative)
  :hook ((c-mode c++-mode asm-mode) . helm-gtags-mode))

;;; magit
(use-package magit
  :ensure t
  :commands (magit-init
             magit-status)
  :bind ("C-x g" . magit-status))

(use-package cmake-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.phtml?\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.jsx$" . web-mode))
  :config
  ;; highlight enclosing tags of the element under cursor
  (setq web-mode-enable-current-element-highlight t)
  :defer 5)

(use-package flycheck
  :ensure t
  :defer 2
  :diminish
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'json-mode-hook 'enable-paredit-mode))

(load "./kconfig.el")

(require 'camelcase-settings)

(require 'lang-elisp)
(require 'lang-python)
(require 'lang-go)
(require 'lang-kotlin)

;;; For JavaScript
;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure    t
  :bind (:map json-mode-map
              ("C-c i" . json-mode-beautify)
              ("{" . paredit-open-curly)
              ("}" . paredit-close-curly))
  :mode ("\\.\\(json\\)$" . json-mode))

(use-package tern
  :ensure t
  :diminish tern-mode
  :defer 5
  :config (add-hook 'js2-mode-hook 'tern-mode))

(use-package company-tern
  :ensure t
  :after (company tern)
  :defer 5
  :config (add-to-list 'company-backends 'company-tern))

(use-package js2-mode
  :ensure t
  :after tern
  :defer 5
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init (setq js2-highlight-level 3
              js2-strict-trailing-comma-warning nil
              js2-strict-missing-semi-warning nil
              js2-missing-semi-one-line-override t
              js2-allow-rhino-new-expr-initializer nil
              js2-include-node-externs t
              js2-warn-about-unused-function-arguments t
              js2-basic-offset 2)

  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (subword-mode 1)
                             (diminish 'subword-mode)))
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook 'paredit-everywhere-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
  (use-package js2-refactor
    :ensure t
    :diminish js2-refactor-mode
    :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c r")))



(use-package paredit-everywhere
  :ensure t
  :after (js2-mode)
  :bind (:map js2-mode-map
              ("{" . paredit-open-curly)
              ("}" . paredit-close-curly-and-newline)))

(use-package dts-mode
  :ensure t
  :defer 5
  :mode
  ("\\.dts\\'" . dts-mode)
  ("\\.dtsi\\'" . dts-mode))

(use-package bitbake
  :ensure t
  :defer 7
  :mode
  ("\\.bb\\'" . bitbake-mode)
  ("\\.bbappend\\'" . bitbake-mode))

;; coding rules
(load-file (concat custom-dir "/coding-rule-bnsoft.el")) ; for BNSoft's C coding rule

;; GNU Global
;; (when-windows
;;   (add-to-list 'load-path "c:/pkg/global/share/gtags/"))
;; (autoload 'gtags-mode "gtags" "Loading GNU Global" t)

;; copied from `http://emacs-fu.blogspot.kr/2009/01/navigating-through-source-code-using.html'
(defun gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory)
            (topdir (read-directory-name
                     "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir))             ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update-single (filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

;; (add-hook 'after-save-hook 'gtags-update-hook)

;; (add-hook 'gtags-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "M-.") 'gtags-find-tag)
;;             (local-set-key (kbd "M-,") 'gtags-find-rtag)))

;; (add-hook 'gtags-select-mode-hook
;;           '(lambda ()
;;              (setq hl-line-face 'underline)
;;              (hl-line-mode 1)
;;              ))

;; (setq gtags-suggested-key-mapping t)

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; (gtags-mode 1)
;;             (gtags-create-or-update)))

;; -----------------------------------------------------------------------------
;; hideshow for programming
;; -----------------------------------------------------------------------------
(load-library "hideshow")
;; hide상태에서 goto-line했을 때 자동으로 show로 변경
(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))
            (c-add-style
             "linux-notab"
             '("linux" (c-offsets-alist
                        (c-lineup-C-comments))
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)))))

;; c mode
(use-package cc-mode
  :config
  (add-hook 'c-mode-hook
	    (lambda ()
	      (let ((filename (buffer-file-name)))
		;; Enable kernel mode for the appropriate files
                (if (and filename
			   (string-match (expand-file-name "~/work")
					 filename))
		  (progn (setq indent-tabs-mode t)
		   (setq show-trailing-whitespace t)
		   (c-set-style "linux-tabs-only"))
		  (c-set-style "linux-notab")))
	      (hs-minor-mode)
	      (rainbow-delimiters-mode)))

  (add-hook 'c++-mode-hook '
	    (lambda ()
	      (bnsoft-c-mode-common-hook)
	      (hs-minor-mode)
	      (rainbow-delimiters-mode))))

;; (add-hook 'c-mode-hook '
;;           (lambda ()
;;             (bnsoft-c-mode-common-hook)
;;             (hs-minor-mode)))


;; (add-hook 'c-mode-hook 'setnu-mode)     ; line number

;; ANSI colors for the compilation mode
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (semantic-mode 1))

(use-package ede
  :config
  ;; Enable EDE only in C/C++
  (global-ede-mode))

;; java mode hook
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

;; (global-set-key [f5] 'hs-toggle-hiding)
;; (global-set-key [f6] 'hs-show-all)
;; (global-set-key [f7] 'hs-hide-all)


;; (fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
;; (fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.
;; (global-set-key "\M-]" 'find-next-tag)
;; (global-set-key "\M-[" 'find-prev-tag)
(global-set-key [C-return] 'semantic-complete-analyze-inline)

;; ;; -----------------------------------------------------------------------------
;; ;; slime
;; ;; -----------------------------------------------------------------------------
;; (add-to-list 'load-path (concat plugins-dir "/slime"))

;; (when-linux
;;  (setq inferior-lisp-program "~/cl/bin/sbcl"))

;; (when-windows
;;  (setq inferior-lisp-program "C:/devel/sbcl/1.1.8/sbcl.exe"))

;; ;; lisp-indent-function 'common-lisp-indent-function
;; ;; slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;; ;; slime-startup-animation nil)

;; (require 'slime)
;; (slime-setup '(slime-fancy slime-fuzzy slime-c-p-c))
;; (setq slime-net-coding-system 'utf-8-unix)

;; (setq common-lisp-hyperspec-root "http://www.lispworks.com/documentation/HyperSpec/")
;; (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)

;; (defun slime-common-lisp ()
;;   (interactive)
;;   ;; (when-linux
;;   ;;  (setq inferior-lisp-program "/usr/bin/sbcl"))
;;   ;; (when-windows
;;   ;;  (setq inferior-lisp-program "C:/pkg/clisp-2.49/clisp.exe"))
;;   ;; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")
;;   (require 'slime)
;;   (slime-setup '(slime-repl))
;;   (slime))

;; (defun slime-clojure ()
;;   (interactive)
;;   ;; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")
;;   (require 'slime)
;;   (slime-setup '(slime-repl))
;;   (slime-connect "localhost" 4005))

;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


(use-package arduino-mode
  :ensure t
  :requires company
  :config
  (use-package company-arduino :ensure t))

(use-package qt-pro-mode
  :ensure t
  :mode ("\\.pro\\'" "\\.pri\\'"))

(use-package qml-mode
  :ensure t
  :mode ("\\.\\(qml\\)$" . qml-mode))

(provide 'bj-programming)
;;; bj-programming ends here
