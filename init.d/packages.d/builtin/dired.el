(use-package dired
  :config
  (setq dired-use-ls-dired nil))

(use-package dired-x
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files
        (rx (or
             ;; Any file whose name starts with a "."
             (group bol "." (not (any ".")))
             ;; Any sort of Python cache.
             "__pycache__")))
  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode 1))))
