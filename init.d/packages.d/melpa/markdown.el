;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :ensure t
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc")))

;;; markdown.el ends here
