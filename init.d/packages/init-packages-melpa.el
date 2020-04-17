;;; init-packages-melpa.el -- Load and configure packages from public repos.

;;; Commentary:
;;
;; init-packages-melpa.el loads and configures personal and third party
;; packages that live in public package repositories such as elpa and melpa.

;;; Code:

(require 'is-a)
(require 'init-local)


;; Third-party packages from elpa/melpa
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-."     . mc/mark-next-like-this)
  ("C-,"     . mc/mark-previous-like-this)
  ("C->"     . mc/mark-all-like-this)
  ("C-c ."   . mc/mark-all-like-this-dwim)
  ("C-c C-." . mc/edit-lines))
(use-package neotree
  :ensure t
  :config
  (add-to-list 'neo-hidden-regexp-list "__pycache__")
  (add-to-list 'neo-hidden-regexp-list ".*\\.egg-info")
  :bind
  ("<f8>"    . neotree)
  ("C-<tab>" . neotree)
  ("C-<f8>"  . neotree-dir)
  ("s-<f8>"  . neotree-dir)
  :custom
  (neo-theme 'icons)
  (neo-window-width 30))
(use-package package-lint
  :ensure t)
(use-package page-break-lines
  :ensure t
  :diminish
  :commands global-page-break-lines-mode
  :init
  (global-page-break-lines-mode))
(use-package paren-face
  :ensure t
  :demand
  :commands global-paren-face-mode
  :config
  (global-paren-face-mode t)
  (set-face-foreground 'parenthesis "gray78"))
(use-package pdf-tools
  :ensure t)
(use-package pipenv
  :ensure t
  :diminish "PE"
  :hook (python-mode . pipenv-mode)
  :commands pipenv-projectile-after-switch-extended
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  :bind
  ("<f12> p a" . pipenv-activate)
  ("<f12> p d" . pipenv-deactivate))
(use-package powerline
  :ensure t
  :custom
  (powerline-default-separator 'utf-8)
  :config
  (when is-a-macOS-p
    (set (intern "ns-use-srgb-colorspace") nil))
  (set-face-background 'powerline-active1 "grey95")
  (set-face-foreground 'powerline-active1 "grey50")
  (set-face-background 'powerline-active2 "grey85")
  (set-face-foreground 'powerline-active2 "grey50")
  (set-face-background 'powerline-inactive1 "grey95")
  (set-face-background 'powerline-inactive2 "grey85")
  (powerline-default-theme))
(use-package pretty-mode
  :ensure t
  :config
  (global-pretty-mode 1))
(use-package projectile
  :ensure t
  :custom
  (projectile-known-projects-file (local-emacs-directory "projectile-bookmarks.eld"))
  (projectile-cache-file (local-emacs-directory "projectile.cache"))
  (projectile-mode-line-prefix " P")
  (projectile-switch-project-action 'neotree-projectile-action)
  (projectile-completion-system 'ivy)
  (projectile-project-root-files '(".projectile" "Pipfile"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))
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
