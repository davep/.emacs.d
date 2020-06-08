(use-package eshell-git-prompt
  :ensure t
  :custom-face
  (eshell-git-prompt-powerline-dir-face
   ((t :background "#3a3a3a" :foregorund "white")))
  (eshell-git-prompt-powerline-clean-face
   ((t :background "green" :foreground "#005f00")))
  (eshell-git-prompt-powerline-not-clean-face
   ((t :background "#d7af5f" :foreground "#626262")))
  :commands eshell-git-prompt-use-theme
  :init
  (eshell-git-prompt-use-theme 'powerline))
