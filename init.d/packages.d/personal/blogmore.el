;; -*- lexical-binding: t; -*-

(use-package blogmore
  :ensure t
  :defer t
  :vc (:url "https://github.com/davep/blogmore.el" :rev :newest)
  :init
  ;; Add an end-of-file marker to any new post.
  (add-hook 'blogmore-new-post-hook #'end-it)
  ;; Always start out working on my personal blog.
  (blogmore-work-on "blog.davep.org")
  ;; Add some useful abbrevs for inserting links commonly-used into my blog
  ;; posts.
  (define-abbrev-table 'markdown-mode-abbrev-table
    '(("bm" "[BlogMore](https://blogmore.davep.dev/)")
      ("bme" "[`blogmore.el`](https://github.com/davep/blogmore.el)")
      ("pblog" "[photoblog](https://seen-by.davep.dev/)")))
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
