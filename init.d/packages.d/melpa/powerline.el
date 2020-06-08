(use-package powerline
  :ensure t
  :custom
  (powerline-default-separator 'utf-8)
  :config
  (when is-a-macOS-p
    (set (intern "ns-use-srgb-colorspace") nil))
  (unless is-a-macOS-dark-mode-window-p
    (set-face-background 'powerline-active1 "grey95")
    (set-face-foreground 'powerline-active1 "grey50")
    (set-face-background 'powerline-active2 "grey85")
    (set-face-foreground 'powerline-active2 "grey50")
    (set-face-background 'powerline-inactive1 "grey95")
    (set-face-background 'powerline-inactive2 "grey85"))
  (powerline-default-theme))
