;;; bj-ui.el

(defun bj-ui-load-theme (theme)
  (interactive)
  (if (>= emacs-major-version 24)
      ;; see http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
      (load-theme theme t)
    (error "Can't load the theme, mismatch with the emacs version.")))

(defun bj-ui-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(global-set-key [f11] 'bj-ui-fullscreen)


(setq inhibit-splash-screen t)

;; starting with fullscreen.
; (bjh-ui-fullscreen)

;; load the color theme. ex) zenburn, tango-2, solarized-light,
;; solarized-dark, tron...
(bj-ui-load-theme 'solarized-dark)

;; ;; Turn off early to avoid momentary display.
;; (mapc
;;  (lambda (mode)
;;    (if (fboundp mode)
;;        (funcall mode -1)))
;;  '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; remove the tool-bar
(when window-system (tool-bar-mode -1))

;; face setting
(when window-system
  ;; (set-face-font 'default "-outline-Bitstream Vera Sans Mono-normal-normal-normal-mono-15-*-*-*-c-*-iso10646-1")
  ;; (set-face-font 'default "-outline-DejaVu Sans Mono-normal-normal-normal-mono-15-*-*-*-c-*-iso10646-1")
  (set-face-font 'default "-outline-Monaco-normal-normal-normal-*-*-*-*-*-p-*-iso10646-1")
  ;; (set-face-font 'default "-outline-나눔고딕코딩-normal-normal-normal-mono-16-*-*-*-c-*-iso10646-1")
  ;; (set-face-font 'default "-unknown-나눔고딕코딩-bold-normal-normal-*-*-*-*-*-d-0-iso10646-1")
  ;; (set-face-font 'default "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-fontset-consolas14")
  ;; (set-face-font 'default "-outline-Consolas-normal-normal-normal-mono-15-*-*-*-c-*-iso10646-1")
  ;; (set-face-font 'default "-microsoft-Consolas-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  ;; (set-fontset-font "fontset-default" 'hangul '("malgun gothic" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'hangul '("나눔고딕코딩" . "unicode-bmp"))
  (when (equal current-language-environment "Korean")
    (set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("NanumGothicCoding" . "unicode-bmp"))) ;; unicode region of Hangul
  ;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc)  '("malgun gothic" . "unicode-bmp")) ;; unicode region of Hangul
  (set-fontset-font "fontset-default" 'kana '("ms mincho" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'han '("ms mincho" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'cjk-misc '("ms mincho" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)  '("new gulim" . "unicode-bmp")) ;; 유니코드 사용자 영역
  )

(provide 'bj-ui)
