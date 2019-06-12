;;; init-editing.el --- Configuration that affects how editing happens.

;;; Commentary:
;;
;; init-editing.el configures how actual editing happens. Note that any
;; package-based config will always be done via `use-package' in the
;; relevant init-package-* file, as a preference. This file is only used for
;; those changes that don't directly relate to a package.

;;; Code:

(setq-default
 ;; https://stackoverflow.blog/2017/06/15/developers-use-spaces-make-money-use-tabs/
 indent-tabs-mode nil
 ;; Never been a fan of double spaces after a full stop.
 sentence-end-double-space nil
 ;; Have sorting be case-insensitive.
 sort-fold-case t)

(provide 'init-editing)

;;; init-editing.el ends here
