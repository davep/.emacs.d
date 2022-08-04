(use-package poetry
  :ensure t
  :commands poetry
  :init
  (poetry-tracking-mode)
  :bind
  ("<f12> p o". poetry))

