;;; init-packages.el --- Package loading

;;; Commentary:
;;
;; init-packages.el is reponsible for loading the packages I want. This
;; makes very heavy use of use-package and is designed to help bootstrap a
;; fresh Emacs installation.
;;
;; http://blog.davep.org/2017/04/01/another_revamp_of_my_emacs_config.html
;; contains a bit of background.

;;; Code:

;; Ensure local-emacs-directory is available.
(require 'init-local)

;; Ensure use-package is available.
(require 'use-package)

;; Ensure diminish is available.
(use-package diminish :ensure t)

;; Load in all the packages declared in the "use" hierarchy.
(let ((source (expand-file-name "init.d/packages.d/" user-emacs-directory)))
  (when (file-exists-p source)
    (cl-loop for use in (directory-files-recursively source (rx ".el" eol))
             do (load (file-name-sans-extension use)))))

(provide 'init-packages)

;;; init-packages.el ends here
