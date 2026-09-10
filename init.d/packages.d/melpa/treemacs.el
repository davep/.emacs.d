;; -*- lexical-binding: t; -*-

(require 'init-local)

(use-package treemacs
  :ensure t
  :defer t
  :hook (emacs-startup . treemacs-start-on-boot)
  :custom
  (treemacs-git-mode 'deferred)
  (treemacs-persist-file (local-emacs-directory "treemacs-persist"))
  (treemacs-select-when-already-in-treemacs 'move-back)
  (treemacs-show-hidden-files nil)
  (treemacs-space-between-root-nodes nil)
  (treemacs-width 30)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-commit-diff-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode 1)
  (when (executable-find "git")
    (treemacs-git-mode (if (executable-find "python3") 'deferred 'simple)))
  (add-to-list 'treemacs-ignored-file-predicates
               (lambda (file _)
                 (or (string= file "__pycache__")
                     (string-suffix-p ".egg-info" file))))
  (defun my/treemacs-switch-workspace-for-current-buffer (&optional _frame-or-window)
    "Switch the active workspace if the selected buffer belongs to a different one."
    (when-let* ((file (buffer-file-name (buffer-base-buffer)))
                ((file-exists-p file))
                (current-ws (treemacs-current-workspace)))
      (unless (treemacs-is-path file :in-workspace current-ws)
        (cl-loop for ws in (treemacs-workspaces)
                 thereis (when (treemacs-is-path file :in-workspace ws)
                           (treemacs-do-switch-workspace ws)
                           t)))))
  ;; Run whenever the active window changes or a new buffer is displayed
  (add-hook 'window-selection-change-functions #'my/treemacs-switch-workspace-for-current-buffer)
  :bind
  (:map global-map
        ("C-<tab>"   . treemacs-select-window)
        ("<f8>"      . treemacs)
        ("C-<f8>"    . treemacs-select-directory)
        ("s-<f8>"    . treemacs-select-directory)
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t C-t" . treemacs-find-file))
  (:map treemacs-mode-map
        ("<home>"    . treemacs-goto-parent-node)
        ("<next>"    . treemacs-next-project)
        ("<prior>"   . treemacs-previous-project)))

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
