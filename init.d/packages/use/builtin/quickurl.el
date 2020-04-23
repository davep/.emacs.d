(use-package quickurl
  :bind
  ("<f12> u u" . quickurl)
  ("<f12> u l" . quickurl-list)
  :custom
  (quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
