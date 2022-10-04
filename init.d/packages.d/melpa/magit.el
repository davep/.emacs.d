(require 'is-a)
(require 'init-local)

(when is-a-unix-p

  (use-package magit
    :ensure t
    :commands magit-after-save-refresh-status
    :config
    (add-hook 'after-save-hook #'magit-after-save-refresh-status)
    (add-hook 'git-commit-setup-hook (lambda() (flyspell-mode 1)))
    (setq ghub-use-workaround-for-emacs-bug 'force)
    :bind
    ("<f12> g s"   . magit-status)
    ("<f12> g b"   . magit-blame)
    ("<f12> g l a" . magit-log-all)
    ("<f12> g l f" . magit-log-buffer-file))

  (use-package forge
    :ensure t
    :custom
    (forge-database-file (local-emacs-directory "forge-database.sqlite"))
    :config
    (push (list "gitlab.synpromics.com"
                "gitlab.synpromics.com/api/v4"
                "gitlab.synpromics.com"
                'forge-gitlab-repository)
          forge-alist))

  (use-package transient
    :ensure t
    :custom
    (transient-history-file (local-emacs-directory "transient-history.el"))
    (transient-levels-file (local-emacs-directory "transient-levels.el")))

  (use-package transient-posframe
    :ensure t
    :custom
    (transient-posframe-border-width 3)
    (transient-posframe-parameters
     '((left-fringe . 8)
       (right-fringe . 8)))
    :init (transient-posframe-mode 1)))

