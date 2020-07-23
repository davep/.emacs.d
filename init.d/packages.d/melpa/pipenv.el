(use-package pipenv
  :ensure t
  :diminish "PE"
  :hook (python-mode . pipenv-mode)
  :commands pipenv-projectile-after-switch-extended
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  ;; Pipenv via Homebrew is having some issues at the moment, which throws
  ;; off this code. So let's work around that.
  (setenv "PIPENV_VERBOSITY" "-1")
  :bind
  ("<f12> p a" . pipenv-activate)
  ("<f12> p d" . pipenv-deactivate))
