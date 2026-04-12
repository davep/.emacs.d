;; -*- lexical-binding: t; -*-

(use-package boxquote
  :ensure t
  :bind
  ("<f12> x i"   . boxquote-insert-file)
  ("<f12> x M-w" . boxquote-kill-ring-save)
  ("<f12> x y"   . boxquote-yank)
  ("<f12> x b"   . boxquote-region)
  ("<f12> x t"   . boxquote-title)
  ("<f12> x h f" . boxquote-describe-function)
  ("<f12> x h v" . boxquote-describe-variable)
  ("<f12> x h k" . boxquote-describe-key)
  ("<f12> x h w" . boxquote-where-is)
  ("<f12> x !"   . boxquote-shell-command))

;;; boxquote.el ends here
