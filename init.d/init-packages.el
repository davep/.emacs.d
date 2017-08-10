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

;; Ensure use-package is available.
(require 'use-package)

;; Given I have my own package archive, ensure I can manage that too.
(let ((delpa "~/develop/elisp/delpa/"))
  (use-package package-x
    :if (file-exists-p delpa)
    :config
    (setq package-archive-upload-base delpa)))

;; Add the package loading directory to the load-path.
(push (locate-user-emacs-file "init.d/packages/") load-path)

;; Use all the packages.
(require 'init-packages-builtin)        ; Things built into Emacs.
(require 'init-packages-delpa)          ; My personal archive.
(require 'init-packages-melpa)          ; Melpa.

(provide 'init-packages)

;;; init-packages.el ends here
