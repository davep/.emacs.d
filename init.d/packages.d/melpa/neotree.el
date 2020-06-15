(use-package neotree
  :ensure t
  :config
  (add-to-list 'neo-hidden-regexp-list "__pycache__")
  (add-to-list 'neo-hidden-regexp-list ".*\\.egg-info")
  :bind
  ("<f8>"    . neotree)
  ("C-<tab>" . neotree)
  ("C-<f8>"  . neotree-dir)
  ("s-<f8>"  . neotree-dir)
  :custom
  (neo-theme 'icons)
  (neo-window-width 30))
