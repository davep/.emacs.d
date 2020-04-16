(use-package company
  :ensure t
  :diminish
  :bind
  ("s-SPC" . company-complete)
  :commands global-company-mode
  :hook (after-init . global-company-mode))
