(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :commands counsel-mode
  :custom
  (counsel-find-file-ignore-regexp
   (rx
    (or
     ;; Ignore hidden files.
     (group bol ".")
     ;; I never want to edit the desktop.
     (group "Desktop/" eol)
     ;; Ignore compiled files.
     (group "." (or "pyc" "elc") eol)
     (group ".egg-info/" eol))))
  :init
  (counsel-mode 1)
  :bind
  ("<f12> f g" . counsel-rg)
  ("<f12> f b" . counsel-bookmark)
  ("<f12> f l" . counsel-locate)
  ("C-c C-f" . counsel-recentf)
  ("<f12> f r" . counsel-recentf)
  ("<f12> i c" . counsel-unicode-char)
  ("<f12> f M-x" . counsel-command-history)
  ("<f12> f c w" . counsel-colors-web)
  ("<f12> f c e" . counsel-colors-emacs))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :init
  (counsel-projectile-mode)
  :custom
  (counsel-projectile-switch-project-action 'neotree-dir))

(use-package ivy
  :ensure t
  :diminish
  :commands
  ivy-mode
  ivy-resume
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "[%d/%d] ")
  (ivy-height 15)
  :init
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-mode 1)
  :bind
  ("C-c C-r" . ivy-resume))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package ivy-rich
  :ensure t
  :after ivy
  :commands
  ivy-rich-mode
  :init
  (ivy-rich-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind
  ("C-s" . swiper))

(use-package ivy-posframe
  :ensure t
  :diminish
  :custom
  (ivy-posframe-border-width 3)
  (ivy-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)))
  :init
  (ivy-posframe-mode 1))
