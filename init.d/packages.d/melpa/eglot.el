(use-package eglot
  :ensure t
  :defer t
  :after company
  :hook (python-mode . eglot-ensure))
