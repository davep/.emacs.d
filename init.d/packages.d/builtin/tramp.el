;; -*- lexical-binding: t; -*-

(use-package tramp
  :init
  (setq tramp-persistency-file-name (local-emacs-directory "tramp.el"))
  :custom
  (tramp-default-method "ssh"))

;;; tramp.el ends here
