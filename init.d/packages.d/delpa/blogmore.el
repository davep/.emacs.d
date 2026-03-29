(use-package blogmore
  :vc (:url "https://github.com/davep/blogmore.el" :rev :newest)
  :init (add-hook 'blogmore-new-post-hook #'end-it)
  :custom
  (blogmore-blogs
   '(("blog.davep.org" "~/write/davep.github.com/content/posts/")
     ("seen-by.davep.dev" "~/write/seen-by/content/posts/")))
  :bind
  ("<f12> m b" . blogmore-work-on)
  ("<f12> m p n" . blogmore-new)
  ("<f12> m p e" . blogmore-edit)
  ("<f12> m s c" . blogmore-set-category)
  ("<f12> m a t" . blogmore-add-tag)
  ("<f12> m u d" . blogmore-update-date)
  ("<f12> m u m" . blogmore-update-modified)
  ("<f12> m l p" . blogmore-link-post)
  ("<f12> m l c" . blogmore-link-category)
  ("<f12> m l t" . blogmore-link-tag))
