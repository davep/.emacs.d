;; -*- lexical-binding: t; -*-

(require 'init-local)

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-width 30)
  (treemacs-persist-file (local-emacs-directory "treemacs-persist"))
  (treemacs-select-when-already-in-treemacs 'move-back)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 'always)
  (when (executable-find "git")
    (treemacs-git-mode (if (executable-find "python3") 'deferred 'simple)))
  (add-to-list 'treemacs-ignored-file-predicates
               (lambda (file _)
                 (or (string= file "__pycache__")
                     (string-suffix-p ".egg-info" file))))
  :bind
  (:map global-map
        ("C-<tab>"   . treemacs-select-window)
        ("<f8>"      . treemacs)
        ("C-<f8>"    . treemacs-select-directory)
        ("s-<f8>"    . treemacs-select-directory)
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t C-t" . treemacs-find-file)))

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(defun treemacs-projectile-action (&optional project)
  "Switch to PROJECT (or current project) and display it in Treemacs."
  (interactive)
  (require 'treemacs)
  (if-let* (project
            (projectile-switch-project-action #'treemacs-add-and-display-current-project))
      (counsel-projectile-switch-project-by-name project)
    (treemacs-add-and-display-current-project)))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;;; treemacs.el ends here
