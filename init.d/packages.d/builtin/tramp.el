(use-package tramp
  :custom
  (tramp-default-method        "ssh")
  (tramp-persistency-file-name (local-emacs-directory "tramp.el")))
