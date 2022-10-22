(use-package company
  :ensure t
  :diminish
  :custom
  (company-icon-margin 4)
  (company-tooltip-align-annotations t)
  :bind
  ("s-SPC" . company-complete)
  :commands global-company-mode
  :hook (after-init . global-company-mode))
