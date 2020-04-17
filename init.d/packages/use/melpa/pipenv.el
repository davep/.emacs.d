(use-package pipenv
  :ensure t
  :diminish "PE"
  :hook (python-mode . pipenv-mode)
  :commands pipenv-projectile-after-switch-extended
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  :bind
  ("<f12> p a" . pipenv-activate)
  ("<f12> p d" . pipenv-deactivate))
