;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up auto-compile.
(require 'auto-compile)
(auto-compile-on-save-mode)             ; Compile on save.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Have ibuffer auto-update the list.
(add-hook 'ibuffer-mode-hooks
          '(lambda ()
            (ibuffer-auto-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footnote management.
(eval-when-compile
  (require 'footnote))
(add-hook 'footnote-mode-hook
          #'(lambda ()
              (setq footnote-style 'numeric-latin
                    footnote-spaced-footnotes nil
                    footnote-section-tag "-----"
                    footnote-section-tag-regexp (regexp-quote footnote-section-tag)
                    footnote-narrow-to-footnotes-when-editing t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoload things that don't correctly do autoload.
(autoload 'hyde "hyde" "hyde" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make it harder to kill an emacs server instance.
(defun long-term-emacs ()
  "Turn this emacs sesssion into a long term emacs.

This involves disabling C-x C-c and also calling `server-start'."
  (interactive)
  (server-start)
  (define-key global-map [(control x) (control c)]
    #'(lambda ()
        (interactive)
        (message "C-x C-c is disabled"))))

(provide 'davep-misc)
