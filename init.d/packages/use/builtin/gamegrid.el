(require 'init-local)

(use-package gamegrid
  :config
  (setq gamegrid-user-score-file-directory (local-emacs-directory "games/")))
