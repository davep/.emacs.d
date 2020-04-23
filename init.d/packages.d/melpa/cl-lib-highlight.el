(use-package cl-lib-highlight
  :ensure t
  :commands
  cl-lib-highlight-initialize
  cl-lib-highlight-warn-cl-initialize
  :init
  (cl-lib-highlight-initialize)
  (cl-lib-highlight-warn-cl-initialize))
