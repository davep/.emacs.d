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
   (list
    (blogmore-blog
     :title "blog.davep.org"
     :posts-directory "~/write/davep.github.com/content/posts/"
     :post-subdirectory-function (lambda () (format-time-string "%Y/%m/")))
    (blogmore-blog
     :title "seen-by.davep.dev"
     :posts-directory "~/write/seen-by/content/posts/")))
  :bind
  ("<f12> b" . blogmore))

;;; blogmore.el ends here
