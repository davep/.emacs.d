(use-package become
  :ensure t
  :commands become-free-of-trailing-whitespace
  :init
  (unless noninteractive
    (add-hook 'before-save-hook #'become-free-of-trailing-whitespace))
  :bind
  ("<f12> <tab>" . become-freshly-indented-no-tabs))
