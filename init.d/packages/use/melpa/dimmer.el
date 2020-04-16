(use-package dimmer
  :ensure t
  :config
  (add-to-list 'dimmer-buffer-exclusion-regexps (rx bol " *NeoTree*" eol))
  :init
  (dimmer-configure-magit)
  (dimmer-mode 1))
