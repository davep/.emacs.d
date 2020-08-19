(use-package powerline
  :ensure t
  :custom
  (powerline-default-separator 'utf-8)
  :config
  (when is-a-macOS-p
    (set (intern "ns-use-srgb-colorspace") nil))
  (powerline-default-theme))
