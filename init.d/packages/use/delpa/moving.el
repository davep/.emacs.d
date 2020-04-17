(use-package moving
  :ensure t
  :bind
  ("C-a"       . moving-home)
  ("<home>"    . moving-home)
  ("s-<left>"  . moving-home)
  ("C-e"       . moving-end)
  ("<end>"     . moving-end)
  ("s-<right>" . moving-end)
  ("s-<up>"    . moving-backward-page)
  ("s-<down>"  . moving-forward-page))
