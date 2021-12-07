(use-package multiple-cursors
  :ensure t
  :bind
  ("s-."     . mc/mark-next-like-this)
  ("s-,"     . mc/mark-previous-like-this)
  ("s-*"     . mc/mark-all-like-this)
  ("s-@"     . mc/mark-all-like-this-dwim)
  ("C-c C-." . mc/edit-lines))
