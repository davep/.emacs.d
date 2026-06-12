;; -*- lexical-binding: t; -*-

(use-package mood-line
  :ensure t
  :config
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
        (mood-line-defformat
         :left
         ((mood-line-segment-buffer-name)
          " "
          (or (mood-line-segment-buffer-status) ":")
          " "
          (mood-line-segment-major-mode))
         :right
         ((mood-line-segment-vc)
          " "
          (mood-line-segment-cursor-position))))
  (mood-line-mode 1))

;;; mood-line.el ends here
