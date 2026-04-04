;; -*- lexical-binding: t; -*-

(use-package blogmore
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/blogmore.el" :rev :newest)
  :init
  (add-hook 'blogmore-new-post-hook #'end-it)
  (blogmore-work-on "blog.davep.org")
  :custom
  (blogmore-blogs
   '(("blog.davep.org" "~/write/davep.github.com/content/posts/")
     ("seen-by.davep.dev" "~/write/seen-by/content/posts/")))
  :bind
  ("<f12> b" . blogmore))

;;; blogmore.el ends here
