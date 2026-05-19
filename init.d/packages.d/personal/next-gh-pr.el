;; -*- lexical-binding: t; -*-

(use-package next-gh-pr
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/next-gh-pr.el" :rev :newest)
  :bind
  ("<f12> i r" . next-gh-pr-insert-markdown-link))

;;; insert.el ends here
