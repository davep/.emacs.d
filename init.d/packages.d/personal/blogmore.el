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
   '(("blog.davep.org"
      ;; Root directory for posts.
      "~/write/davep.github.com/content/posts/"
      ;; Subdirectory for new posts, relative to the root.
      (lambda () (format-time-string "%Y/%m/")))
     ("seen-by.davep.dev"
      ;; Root directory for posts.
      "~/write/seen-by/content/posts/")))
  :bind
  ("<f12> b" . blogmore))

;;; blogmore.el ends here
