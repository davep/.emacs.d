(use-package beacon
  :if (display-graphic-p)
  :ensure t
  :diminish
  :commands beacon-mode
  :config (beacon-mode 1))
