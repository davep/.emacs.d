;; -*- lexical-binding: t; -*-

(use-package gitweb
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/gitweb.el" :rev :newest)
  :bind
  ("<f12> g w" . gitweb))

;;; gitweb.el ends here
