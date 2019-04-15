;;; bj-common.el

(defun bj-use-package (name)
  "If feature NAME is not installed, install it from the ELPA;
then load it."
  (interactive)
  (when-version-24
   (if (not (require name nil t))
       (package-install name)
     t)
   (require name nil t)))

;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

;; for iswitchb
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

;; Terminal at Your Fingertips. http://emacsredux.com/blog/2013/03/29/terminal-at-your-fingertips
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
       (split-window-sensibly (selected-window))
       (other-window 1)
       (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)


;; env for Windows
(when-windows
 (let* ((cygwin-root "c:/cygwin64")
        (cygwin-bin (concat cygwin-root "/bin"))
        (cygwin-usr-bin (concat cygwin-root "/usr/bin")))

   (setenv "PATH" (concat "C:/win-builds/bin" path-separator
                          (getenv "PATH")))
   (add-to-list 'exec-path "C:/local/bin")
   (add-to-list 'exec-path "C:/win-builds/bin")
   (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")))

(when-mac
 (progn
   (setenv "PATH" (concat "~/.local/bin" path-separator
                          "/usr/local/bin" path-separator
                          "/Library/TeX/texbin" path-separator
                          (getenv "PATH")))
   (add-to-list 'exec-path "/usr/local/bin")
   (add-to-list 'exec-path "/Library/TeX/texbin")
   (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
   ;; refer to https://emacs.stackexchange.com/a/29101
   (setq insert-directory-program "gls" dired-use-ls-dired t)))

(when-linux
 (progn
   (setenv "PATH" (concat "~/.local/bin" path-separator
                          "/usr/local/bin" path-separator
                          (getenv "PATH")))
   (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
   (setenv "SBCL_HOME" (expand-file-name "~/cl/lib/sbcl"))))

(when-windows
 (let ((bash-dir "C:/Program Files/Git/bin"))
   (setq explicit-shell-file-name (concat bash-dir "/bash.exe"))
   (setq shell-file-name explicit-shell-file-name)
   (add-to-list 'exec-path bash-dir)
   (setq explicit-bash-args '("--login" "-i"))
   (setenv "SHELL" explicit-shell-file-name)
   (setenv "PATH" (concat bash-dir path-separator (getenv "PATH")))))

;; Use left/right arrow keys for the iswitchb
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; ANSI colors for the Emacs Shell(eshell)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; key mapping
(global-set-key [f12] 'toggle-truncate-lines) ; F12 to toggle line wrap


;; (bj-use-package 'dired+)
;; (bj-use-package 'htmlize)

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
	 ("C-c t" . crux-visit-term-buffer)))

(provide 'bj-common)
