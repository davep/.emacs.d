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
