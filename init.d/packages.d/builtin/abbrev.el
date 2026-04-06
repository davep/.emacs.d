;; -*- lexical-binding: t; -*-

(require 'init-local)

(use-package abbrev
  :custom
  (save-abbrevs nil)
  (abbrev-file-name (local-emacs-directory "abbrev_defs.el")))

;;; abbrev.el ends here
