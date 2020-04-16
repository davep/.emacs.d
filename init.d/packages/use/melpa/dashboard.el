(use-package dashboard
  :ensure t
  :demand
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-set-footer     nil
        dashboard-items          '((projects  . 10)
                                   (recents   . 10)
                                   (bookmarks . 10)))
  (dashboard-setup-startup-hook)
  :commands
  dashboard-insert-startupify-lists
  :bind
  ("<f7>" . (lambda ()
              (interactive)
              (when (get-buffer dashboard-buffer-name)
                (kill-buffer dashboard-buffer-name))
              (dashboard-insert-startupify-lists)
              (switch-to-buffer dashboard-buffer-name))))
