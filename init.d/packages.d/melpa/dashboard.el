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
  dashboard-refresh-buffer
  :bind
  ("<f7>" . dashboard-refresh-buffer))
