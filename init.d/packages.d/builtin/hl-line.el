(use-package hl-line
  ;; Turn on for every buffer.
  :init (global-hl-line-mode 1)
  :custom
  ;; Keep the highlight visible in every buffer
  (global-hl-line-sticky-flag t)
  :config
  ;; Turn off for eshell though 'cos it messes with the prompt.
  (add-hook 'eshell-mode-hook (lambda () (setq-local global-hl-line-mode nil))))
