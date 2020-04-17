(use-package multiple-cursors
  :ensure t
  :bind
  ("C-."     . mc/mark-next-like-this)
  ("C-,"     . mc/mark-previous-like-this)
  ("C->"     . mc/mark-all-like-this)
  ("C-c ."   . mc/mark-all-like-this-dwim)
  ("C-c C-." . mc/edit-lines))
