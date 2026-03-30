;; -*- lexical-binding: t; -*-

(use-package geturl
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/geturl.el" :rev :newest)
  :bind
  ("<f12> i u" . geturl-insert))

;;; geturl.el ends here
