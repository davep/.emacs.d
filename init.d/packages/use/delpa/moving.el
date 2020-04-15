(use-package moving
  :ensure t
  :bind
  ("<home>"    . moving-home)
  ("s-<left>"  . moving-home)
  ("<end>"     . moving-end)
  ("s-<right>" . moving-end)
  ("s-<up>"    . moving-backward-page)
  ("s-<down>"  . moving-forward-page))
