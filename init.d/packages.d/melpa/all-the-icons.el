(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :diminish
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :after all-the-icons
  :init (all-the-icons-ibuffer-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
