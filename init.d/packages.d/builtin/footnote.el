(use-package footnote
  :commands footnote-mode
  :config
  (add-hook 'footnote-mode-hook
            (lambda ()
              (setq footnote-style 'numeric-latin
                    footnote-spaced-footnotes nil
                    footnote-section-tag "-----"
                    footnote-section-tag-regexp (regexp-quote footnote-section-tag)
                    footnote-narrow-to-footnotes-when-editing t)))
  :hook (text-mode . footnote-mode))
