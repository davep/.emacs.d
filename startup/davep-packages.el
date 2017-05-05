;;; davep-packages.el --- Package loading

;;; Commentary:
;;
;; davep-packages.el is reponsible for loading the packages I want. This
;; makes very heavy use of use-package and is designed to help bootstrap a
;; fresh Emacs installation.
;;
;; http://blog.davep.org/2017/04/01/another_revamp_of_my_emacs_config.html
;; contains a bit of background.

;;; Code:

;; Ensure use-package is available.
(require 'use-package)

;; Given I have my own package archive, ensure I can manage that too.
(require 'package-x)

;; Add the package loading directory to the load-path.
(push (concat user-emacs-directory "startup/packages/") load-path)

;; Emacs builtin packages, that aren't normally loaded.
(require 'davep-packages-builtin)

;; My personal packages. Normally loaded in from delpa.
(require 'davep-packages-delpa)

;; Packages that live in elpa/melpa.
(require 'davep-packages-melpa)

;; Catch-all hooks, etc.
(add-hook 'text-mode-hook
          (lambda()
            (flyspell-mode 1)
            (footnote-mode 1)))
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode)
            (flyspell-mode 1)))
(add-hook 'emacs-lisp-mode-hook #'setup-compile)

(provide 'davep-packages)

;;; davep-packages.el ends here
