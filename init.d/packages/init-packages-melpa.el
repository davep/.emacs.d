;;; init-packages-melpa.el -- Load and configure packages from public repos.

;;; Commentary:
;;
;; init-packages-melpa.el loads and configures personal and third party
;; packages that live in public package repositories such as elpa and melpa.

;;; Code:

(require 'is-a)
(require 'init-local)


;; Third-party packages from elpa/melpa
(use-package restclient
  :ensure t
  :commands restclient-mode
  :init
  (defun restclient-scratch ()
    "Create a scratch buffer for use with `resctclient-mode'."
    (interactive)
    (switch-to-buffer "*restclient*")
    (when (string= (buffer-string) "")
      (insert "# -*- restclient -*-\n\n"))
    (restclient-mode)))
(use-package rg
  :ensure t
  :bind
  ("<f12> = =" . rg)
  ("<f12> = +" . rg-dwim))
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "ros run")
  (add-hook 'slime-inferior-process-start-hook
            (lambda ()
              (require 'slime-fancy))))
(use-package string-inflection
  :ensure t
  :bind
  ("<f12> s" . string-inflection-all-cycle))
(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  :ensure t)
(use-package wc-mode
  :ensure t)
(use-package wttrin
  :ensure t
  :custom
  (wttrin-default-cities '("Edinburgh"))
  (wttrin-default-accept-language '("Accept-Language" . "en-GB"))
  :bind
  ("<f12> x x" . wttrin))

(provide 'init-packages-melpa)

;;; init-packages-melpa.el ends here
