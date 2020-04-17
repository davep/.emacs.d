(use-package paren-face
  :ensure t
  :demand
  :commands global-paren-face-mode
  :config
  (global-paren-face-mode t)
  (set-face-foreground 'parenthesis "gray78"))
