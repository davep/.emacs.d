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
;; On the Mac local bin doesn't seem to be in the path if I run from
;; the dock. Fix this.
(let ((local "/usr/local/bin"))
  (when (and davep:osx-p (not (member local exec-path)))
    (push local exec-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If we're on a Unix of some sort, add a local bin (if it's there).
(when (and davep:unixp (file-exists-p "~/bin"))
  (push "~/bin/" exec-path))

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
