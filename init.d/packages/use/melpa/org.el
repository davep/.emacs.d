(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-present
  :ensure t
  :after org
  :commands
  org-present-big
  org-present-hide-cursor
  org-present-read-only
  org-present-read-write
  org-present-small
  org-present-show-cursor
  org-display-inline-images
  org-remove-inline-images
  :init
  (progn
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)
              (display-line-numbers-mode 0)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
              (display-line-numbers-mode 1)))))
