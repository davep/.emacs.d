(use-package eshell-git-prompt
  :ensure t
  :custom-face
  (eshell-git-prompt-powerline-dir-face
   ((t :background "grey80")))
  (eshell-git-prompt-powerline-clean-face
   ((t :background "green")))
  (eshell-git-prompt-powerline-not-clean-face
   ((t :background "yellow")))
  :commands eshell-git-prompt-use-theme
  :init
  (eshell-git-prompt-use-theme 'powerline))
