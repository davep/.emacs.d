;; -*- lexical-binding: t; -*-

(use-package unabbrev
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/unabbrev.el" :rev :newest)
  :bind
  ("<f12> a" . unabbrev-picker))

;;; unabbrev.el ends here
