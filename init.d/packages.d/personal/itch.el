;; -*- lexical-binding: t; -*-

(use-package itch
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/itch.el" :rev :newest)
  :bind*
  ("M-s"         . itch-scratch-buffer)
  ("<XF86Eject>" . itch-scratch-buffer))

;;; itch.el ends here
