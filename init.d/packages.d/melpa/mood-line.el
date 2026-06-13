;; -*- lexical-binding: t; -*-

(use-package mood-line
  :ensure t
  :config
  (defface my/mood-line-good-status
    '((t (:foreground "#009900" :weight bold)))
    "Face for good status in mood-line."
    :group 'mood-line)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
        (mood-line-defformat
         :left
         ((or
           (mood-line-segment-buffer-status)
           (propertize
            (mood-line--get-glyph :buffer-modified)
            'face 'my/mood-line-good-status))
          " "
          (mood-line-segment-buffer-name)
          " : "
          (mood-line-segment-major-mode))
         :right
         ((mood-line-segment-vc)
          " "
          (mood-line-segment-cursor-position))))
  (mood-line-mode 1))

;;; mood-line.el ends here
