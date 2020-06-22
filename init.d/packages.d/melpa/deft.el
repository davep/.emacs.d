(use-package deft
  :ensure t
  :custom
  (deft-directory "~/notebook")
  (deft-recursive t)
  (deft-use-filename-as-title t)
  :commands deft
  :bind
  ("<f12> RET" . deft))
