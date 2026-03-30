;; -*- lexical-binding: t; -*-

(use-package moving
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/moving.el" :rev :newest)
  :bind
  ("<home>"    . moving-home)
  ("s-<left>"  . moving-home)
  ("<end>"     . moving-end)
  ("s-<right>" . moving-end)
  ("s-<up>"    . moving-backward-page)
  ("s-<down>"  . moving-forward-page))

;;; moving.el ends here
