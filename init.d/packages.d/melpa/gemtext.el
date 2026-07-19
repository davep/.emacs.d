;; -*- lexical-binding: t; -*-

(use-package gemtext-mode
  :ensure t
  :mode ("\\.gmi\\'" . gemtext-mode)
  :hook
  (gemtext-mode . (lambda ()
                    (auto-fill-mode -1)
                    (visual-line-mode 1)
                    (setq-local fill-column 120)
                    (visual-fill-column-mode 1))))

;;; gemtext.el ends here
