(require 'init-local)

(use-package uptimes
  :ensure t
  :custom
  (uptimes-database (local-emacs-directory "uptimes.el"))
  :bind
  ("<f11>"       . uptimes-current)
  ("<f12> <f11>" . uptimes))
