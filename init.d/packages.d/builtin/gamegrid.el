;; -*- lexical-binding: t; -*-

(require 'init-local)

(use-package gamegrid
  :config
  (setq gamegrid-user-score-file-directory (local-emacs-directory "games/")))

;;; gamegrid.el ends here
