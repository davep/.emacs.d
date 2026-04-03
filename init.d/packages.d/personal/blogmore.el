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
  ("<f12> b b" . blogmore-work-on)
  ("<f12> b n" . blogmore-new)
  ("<f12> b e" . blogmore-edit)
  ("<f12> b d" . blogmore-toggle-draft)
  ("<f12> b s c" . blogmore-set-category)
  ("<f12> b a t" . blogmore-add-tag)
  ("<f12> b u d" . blogmore-update-date)
  ("<f12> b u m" . blogmore-update-modified)
  ("<f12> b l p" . blogmore-link-post)
  ("<f12> b l c" . blogmore-link-category)
  ("<f12> b l t" . blogmore-link-tag))

;;; blogmore.el ends here
