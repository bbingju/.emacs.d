;;; bjh-hangul.el

(when enable-multibyte-characters
  (set-language-environment "Korean")
  (setq-default file-name-coding-system 'utf-8)
  (setq default-input-method "korean-hangul")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  )

(when-mac (require 'ucs-normalize)
	  (set-file-name-coding-system 'utf-8-hfs)
	  (setq default-process-coding-system '(utf-8-hfs . utf-8-hfs)))

(when-windows (set-file-name-coding-system 'euc-kr)
	      (global-set-key (kbd "S-SPC") 'toggle-input-method)
	      (global-set-key (kbd "<Hangul>") 'toggle-input-method)
	      (global-set-key (kbd "<Hangul_Hanja>") 'hangul-to-hanja-conversion))

;; (custom-set-variables
;;  '(default-input-method "korean-hangul"))

;; (set-selection-coding-system
;;  (cond ((eq system-type 'windows-nt) 'utf-8-dos)
;;        (t 'utf-8)))

(provide 'bj-hangul)
