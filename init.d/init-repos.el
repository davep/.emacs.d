;;; init-repos.el --- Set up package loading and repos we'll load from.

;;; Commentary:
;;
;; init-repos.el takes care of setting up which package repos I use, and
;; ensures that the packages that get pulled down are held in the right
;; place. It's also responsible for ensuring that `use-package' gets
;; installed and used.

;;; Code:

(require 'package)

;; Say where I want packages to live.
(setq package-user-dir (local-emacs-directory "packages")
      package-gnupghome-dir (local-emacs-directory "gnupg"))

;; Add melpa and delpa.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("delpa" . "http://blog.davep.org/delpa/"))

;; Initialise the package system.
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'init-repos)

;;; init-repos.el ends here
