(use-package nuke-buffers
  :ensure t
  :config
  (add-to-list 'nuke-buffers-ignore "*node process*")
  (add-to-list 'nuke-buffers-ignore "*JS scratch*")
  (add-to-list 'nuke-buffers-ignore "*JS REPL*")
  :bind
  ("C-M-<f11>"     . nuke-buffers)
  ("C-s-<f11>"     . nuke-buffers)
  ("C-<XF86Eject>" . nuke-buffers))
