;;; bj-programming.el

;; yasnippet
(when (bj-use-package 'yasnippet)
  (yas-global-mode 1))

;; coding rules
(load-file (concat custom-dir "/coding-rule-bnsoft.el")) ; for BNSoft's C coding rule

;; GNU Global
(autoload 'gtags-mode "gtags" "" t)

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

(add-hook 'after-save-hook 'gtags-update-hook)

(add-hook 'gtags-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'gtags-find-tag)
            (local-set-key (kbd "M-,") 'gtags-find-rtag)))

(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))

(setq gtags-suggested-key-mapping t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (gtags-mode 1)
            (gtags-create-or-update)))

;; c mode
(add-hook 'c-mode-hook '
          (lambda ()
            (c-set-style "BNSoft")
            (setq default-tab-width 4)
            (setq c-basic-offset 4)     ; indent use only 4 blank
            (setq indent-tabs-mode nil) ; no tab
            (hs-minor-mode)             ; hideshow
            ))
;; (add-hook 'c-mode-hook 'setnu-mode)     ; line number

;; ANSI colors for the compilation mode
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

;; java mode hook
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil)))

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

(when-linux
 (setq inferior-lisp-program "~/cl/bin/sbcl"))

(when-windows
 (setq inferior-lisp-program "C:/pkg/clisp-2.49/clisp.exe"))

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
  ;; (when-linux
  ;;  (setq inferior-lisp-program "/usr/bin/sbcl"))
  ;; (when-windows
  ;;  (setq inferior-lisp-program "C:/pkg/clisp-2.49/clisp.exe"))
  ;; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")
  (require 'slime)
  (slime-setup '(slime-repl))
  (slime))

(defun slime-clojure ()
  (interactive)
  ;; (add-to-list 'load-path "~/.emacs.d/plugins/slime/")
  (require 'slime)
  (slime-setup '(slime-repl))
  (slime-connect "localhost" 4005))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))



;; emacs lisp mode
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
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; ediff
(setq ediff-split-window-function 'split-window-vertically)


(provide 'bj-programming)
