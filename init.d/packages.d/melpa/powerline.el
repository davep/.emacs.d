;; -*- lexical-binding: t; -*-

(use-package powerline
  :ensure t
  :custom
  (powerline-default-separator 'utf-8)
  (powerline-display-mule-info nil)
  (powerline-display-hud nil)
  :config
  (when is-a-macOS-p
    (set (intern "ns-use-srgb-colorspace") nil))
  (powerline-default-theme))

;;; powerline.el ends here
