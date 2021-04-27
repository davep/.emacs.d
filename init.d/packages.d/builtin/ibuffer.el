(use-package ibuffer
  :bind
  ("M-<f6>" . ibuffer)
  ("C-x C-b" . ibuffer)
  :commands
  ibuffer-auto-mode
  ibuffer-switch-to-saved-filter-groups
  ibuffer-do-sort-by-alphabetic
  :defines
  ibuffer-show-empty-filter-groups
  ibuffer-saved-filter-groups
  :config
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("davep"
           ("*Magit*"      (derived-mode . magit-mode))
           ("Org"          (or
                            (mode . org-mode)
                            (mode . org-agenda-mode)))
           (".emacs.d"     (filename . "/.emacs.d/"))
           ("shell"        (or
                            (mode . sh-mode)
                            (mode . fish-mode)))
           ("Lisp"         (or
                            (mode . lisp-mode)
                            (mode . slime-repl-mode)))
           ("elisp"        (derived-mode . emacs-lisp-mode))
           ("Makefile"     (derived-mode . makefile-mode))
           ("python"       (mode . python-mode))
           ("JavaScript"   (mode . js2-mode))
           ("Julia"        (mode . julia-mode))
           ("Clojure"      (mode . clojure-mode))
           ("Swift"        (mode . swift-mode))
           ("C/C++"        (or
                            (derived-mode . c-mode)
                            (derived-mode . c++-mode)))
           ("Doc-View"     (mode . doc-view-mode))
           ("web"          (or
                            (mode . web-mode)
                            (mode . scss-mode)))
           ("text files"   (or
                            (derived-mode . text-mode)
                            (mode . fasta-mode)))
           ("directories"  (mode . dired-mode))
           ("Help"         (or
                            (name . "\*Help\*")
                            (name . "\*Apropos\*")
                            (name . "\*info\*")))
           ("Internal"     (or
                            (name . "\*Compile-log\*")
                            (name . "\*Buffer List\*")
                            (name . "\*Backtrace\*")
                            (name . "\*Messages\*")
                            (name . "\*Completions\*")
                            (name . "\*Calendar\*")
                            (name . "\*tramp/sudo")
                            (name . "\*Packages\*")
                            (mode . inferior-python-mode)
                            (mode . compilation-mode))))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "davep")
              (ibuffer-do-sort-by-alphabetic))))
