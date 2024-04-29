;;; https://github.com/quelpa/quelpa-use-package
(use-package mode-on-region
  :quelpa (mode-on-region :fetcher github :repo "davep/mor")
  :bind
  ("<f12> RET" . mor-mode-on-region)
  ("<f12> C-<return>" . mor-prev-mode-on-region)
  (:map mor-tmp-buffer-mode)
  ("C-c C-c" . mor-copy-back)
  ("C-c C-x" . mor-close-tmp-buffer))
