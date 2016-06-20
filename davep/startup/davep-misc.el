;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up auto-compile.
(require 'auto-compile)
(auto-compile-on-save-mode)             ; Compile on save.

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

(provide 'davep-misc)
