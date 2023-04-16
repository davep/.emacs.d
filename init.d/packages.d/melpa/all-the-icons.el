(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :diminish
  :after all-the-icons
  :custom (all-the-icons-dired-monochrome nil)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :after all-the-icons
  :init (all-the-icons-ibuffer-mode 1)
  ;; Quick fix for:
  ;; https://github.com/seagle0128/all-the-icons-ivy-rich/issues/33
  ;; Once that's sorted, kill this off.
  :custom (all-the-icons-ivy-rich-icon-size 0.95))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
