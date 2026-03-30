;; -*- lexical-binding: t; -*-

(use-package nuke-buffers
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/nuke-buffers.el" :rev :newest)
  :config
  (add-to-list 'nuke-buffers-ignore "*node process*")
  (add-to-list 'nuke-buffers-ignore "*JS scratch*")
  (add-to-list 'nuke-buffers-ignore "*JS REPL*")
  :bind
  ("C-M-<f11>"     . nuke-buffers)
  ("C-s-<f11>"     . nuke-buffers)
  ("C-<XF86Eject>" . nuke-buffers))

;;; nuke-buffers.el ends here
