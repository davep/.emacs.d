(use-package beacon
  :if (display-graphic-p)
  :ensure t
  :diminish
  :commands beacon-mode
  :config (beacon-mode 1)
  :custom
  (beacon-size 80))
