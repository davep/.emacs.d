(use-package insert
  :ensure t
  :bind
  ("C-<return>" . insert-line-split-keeping-fill-prefix)
  ("<f12> i a"  . insert-autoload-cookie)
  ("<f12> i f"  . insert-filename)
  ("<f12> i m"  . insert-melpa-badge)
  ("<f12> i s"  . insert-sexp-link)
  ("<f12> i y"  . insert-youtube-markdown)
  ("<f12> i ;"  . insert-break-comment))
