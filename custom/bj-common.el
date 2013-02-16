;;; bj-common.el

;; env setting
(when (eq system-type 'windows-nt)
  (setenv "PATH"
	  (concat
	   "C:/MinGW/msys/1.0/bin" ";"
	   "C:/MinGW/bin" ";"
	   "D:/cygwin/usr/local/bin" ";"
	   "D:/cygwin/usr/bin" ";"
	   "D:/cygwin/bin" ";"
	   (getenv "PATH")
	   )
	  )
  (setq exec-path
	;; '("C:/MinGW/msys/1.0/bin/")
	'("D:/cygwin/bin/")
	)
  )

;; -----------------------------------------------------------------------------
;; mode setting
;; -----------------------------------------------------------------------------
(iswitchb-mode 1)
(column-number-mode nil)
(transient-mark-mode t)
(show-paren-mode t)
(setq make-backup-files nil)
(global-auto-revert-mode 1)
(setq truncate-lines t)			; disable line wrapping


;; for iswitchb
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; dired+
(require 'dired+)

(provide 'bj-common)
