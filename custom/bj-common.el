;;; bj-common.el

(defun bj-use-package (name)
  "If feature NAME is not installed, install it from the ELPA;
then load it."
  (interactive)
  (if (>= emacs-major-version 24)
      (if (not (require name nil t))
	  (package-install name)
      t)
    (require name nil t)))

;; for iswitchb
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))


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

;; Use left/right arrow keys for the iswitchb
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; ANSI colors for the Emacs Shell(eshell)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; key mapping
(global-set-key [f12] 'toggle-truncate-lines) ; F12 to toggle line wrap


(bj-use-package 'dired+)
(bj-use-package 'htmlize)


(provide 'bj-common)
