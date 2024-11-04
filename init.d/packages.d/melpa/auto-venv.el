(use-package pyvenv :ensure t)

(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-find-and-activate)
  (add-hook 'focus-in-hook 'auto-virtualenv-find-and-activate)
  (add-hook 'python-mode-hook 'auto-virtualenv-find-and-activate)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-find-and-activate))
