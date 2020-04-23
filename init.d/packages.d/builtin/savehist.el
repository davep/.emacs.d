(use-package savehist
  :commands savehist-mode
  :custom
  (savehist-file (local-emacs-directory "history.el"))
  :init
  (savehist-mode t))
