;;; bj-programming.el

;; yasnippet
(when (bj-use-package 'yasnippet)
  (yas-global-mode 1))

;; -----------------------------------------------------------------------------
;; coding rules
;; -----------------------------------------------------------------------------
(load-file (concat custom-dir "/coding-rule-bnsoft.el")) ; for BNSoft's C coding rule

;; -----------------------------------------------------------------------------
;; c mode
;; -----------------------------------------------------------------------------
;; (require 'cc-mode)
(add-hook 'c-mode-hook '
	  (lambda ()
	    (c-set-style "BNSoft")
	    (setq default-tab-width 4)
	    (setq c-basic-offset 4) ; indent use only 4 blank
	    (setq indent-tabs-mode nil) ; no tab
	    (hs-minor-mode)		; hideshow
	    ))
;; (add-hook 'c-mode-hook 'setnu-mode)     ; line number

;; ANSI colors for the compilation mode
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

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

(add-hook 'c++-mode-hook 'hs-minor-mode)

(global-set-key [f5] 'hs-toggle-hiding)
(global-set-key [f6] 'hs-show-all)
(global-set-key [f7] 'hs-hide-all)


(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.
(global-set-key "\M-]" 'find-next-tag)
(global-set-key "\M-[" 'find-prev-tag)
(global-set-key [C-return] 'semantic-complete-analyze-inline)

;; -----------------------------------------------------------------------------
;; slime
;; -----------------------------------------------------------------------------
(add-to-list 'load-path (concat plugins-dir "/slime"))

(if (eq system-type 'windows-nt)
    (setq inferior-lisp-program "C:/pkg/clisp-2.49/clisp.exe")
  (setq inferior-lisp-program "/usr/bin/sbcl"))

;; lisp-indent-function 'common-lisp-indent-function
;; slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;; slime-startup-animation nil)

(require 'slime)
(slime-setup '(slime-fancy slime-fuzzy slime-c-p-c))
(setq slime-net-coding-system 'utf-8-unix)

(setq common-lisp-hyperspec-root "http://www.lispworks.com/documentation/HyperSpec/")
(define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)

(defun slime-common-lisp ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (setq inferior-lisp-program "C:/pkg/clisp-2.49/clisp.exe")
    (setq inferior-lisp-program "/usr/bin/sbcl"))
  ; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")
  (require 'slime)
  (slime-setup '(slime-repl))
  (slime))

(defun slime-clojure ()
  (interactive)
  ; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")
  (require 'slime)
  (slime-setup '(slime-repl))
  (slime-connect "localhost" 4005))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


;; -----------------------------------------------------------------------------
;; elisp mode
;; -----------------------------------------------------------------------------
(defun goldmund-emacs-lisp-mode-init ()
  (interactive)
  (imenu-add-to-menubar "Functions")
  (define-key emacs-lisp-mode-map [f6] 'eval-buffer)
  (define-key emacs-lisp-mode-map [(meta f6)] 'emacs-lisp-byte-compile-and-load)
  (define-key emacs-lisp-mode-map [return] 'newline-and-indent)
  ;; (define-key emacs-lisp-mode-map [?\C-c?t] 'xsteve-trace-function)
  (modify-syntax-entry ?- "w")
  (hs-minor-mode t)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'goldmund-emacs-lisp-mode-init)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; -----------------------------------------------------------------------------
;; ediff
;; -----------------------------------------------------------------------------
(setq ediff-split-window-function 'split-window-vertically)


(provide 'bj-programming)
