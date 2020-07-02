(when is-a-macOS-p
  (use-package spotlight
    :ensure t
    :bind
    ("<f12> ? s" . spotlight)
    :custom
    (spotlight-default-base-dir "/")))
