(use-package cheat-sh
  :ensure t
  :bind
  ("<f12> / /" . cheat-sh-maybe-region)
  ("<f12> / l" . cheat-sh-list)
  ("<f12> / ?" . cheat-sh-help)
  ("<f12> / s" . cheat-sh-search)
  ("<f12> / t" . cheat-sh-search-topic))
