;; -*- lexical-binding: t; -*-

(use-package become
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/become.el" :rev :newest)
  :commands become-free-of-trailing-whitespace
  :init
  (unless noninteractive
    (add-hook 'before-save-hook #'become-free-of-trailing-whitespace))
  :bind
  ("<f12> <tab>" . become-freshly-indented-no-tabs))

;;; become.el ends here
