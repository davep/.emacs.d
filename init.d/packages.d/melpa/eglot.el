(use-package eglot
  :ensure t
  :defer t
  :after company
  :hook (python-mode . eglot-ensure)
  :bind
  ;; Some of these obviously aren't actual eglot things, but they're things
  ;; that I find most useful when working via eglot.
  ("<f12> e d" . eldoc)
  ("<f12> e r" . eglot-rename)
  ("<f12> e f d" . eglot-find-declaration)
  ("<f12> e f i" . eglot-find-implementation)
  ("<f12> e f t" . eglot-find-typeDefinition)
  ("<f12> e f r" . xref-find-references))
