(use-package dictionary
  :ensure t
  :bind
  ("<f12> f d" . dictionary-lookup-definition)
  ("<f12> / d" . dictionary-search)
  :custom
  (dictionary-server "dict.org"))
