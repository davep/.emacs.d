(use-package blogmore
  :vc (:url "https://github.com/davep/blogmore.el" :rev :newest)
  :init (add-hook 'blogmore-new-post-hook #'end-it))
