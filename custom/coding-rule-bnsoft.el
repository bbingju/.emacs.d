;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding style bnsoft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst bnsoft-c-style
  '(
    (c-offsets-alist . (
			;;string
                        (c . 0)
                        (defun-open . 0)
                        (defun-close . 0)
                        (defun-block-intro . +)
                        (class-open . 0)
                        (class-close . 0)
			;; inline-open
			;; inline-close
			;; extern-lang-open
			;; extern-lang-close
			;; func-decl-cont
                        (topmost-intro . 0)
			;; topmost-intro-cont
			;; member-init-intro
			;; member-init-cont
			;; inher-intro
			;; inher-cont
			;; block-open
                        (block-close . 0)
                        (brace-list-open . 0)
                        (brace-list-close . 0)
                        (brace-list-intro . +)
                        (brace-list-entry . 0)
			;; statement
                        (statement-cont . (c-lineup-gcc-asm-reg c-lineup-arglist))
			;; statement-block-intro
                        (statement-case-intro . ++)
						(statement-case-open . +)
			;; substatement
                        (substatement-open . 0)
                        (case-label . +)
			;; label
			;; do-while-closure
			;; else-closure
			;;			(comment-intro . 0)
                        (arglist-intro . +)
                        (arglist-cont . 0)
			;; arglist-cont-noempty
                        (arglist-close . c-lineup-close-paren)
			;; stream-op
			;; inclass
			;; inextern-lang
			;; cpp-macro
			;; friend
			;; objc-method-intro
			;; objc-method-args-cont
			;; objc-method-call-cont
                        )
                     )
    )
  "BNSoft Coding Style"
  )

(c-add-style "BNSoft" bnsoft-c-style)

(defun bnsoft-c-mode-common-hook()
  (c-set-style "BNSoft")
  (setq tab-width 4
        indent-tabs-mode nil)
  (setq-default c-basic-offset 4))
