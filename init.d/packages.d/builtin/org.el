(use-package org
  :custom
  (org-directory (or (getenv "ORG_DIRECTORY") "~/notebook/"))
  (org-default-notes-file (concat (file-name-as-directory org-directory) "inbox.org"))
  (org-agenda-files (list org-default-notes-file))
  (org-link-file-path-type 'relative)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-cycle-separator-lines 1)
  :config
  (defun org-davep-config ()
    "Load up the rest of my org-mode config."
    (interactive)
    (let ((org-local-config "~/.config/org/"))
      (when (file-exists-p org-local-config)
        (mapc #'load (directory-files org-local-config t (rx ".el" eol))))))
  (funcall 'org-davep-config)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  :bind
  ("<f12> o a" . org-agenda)
  ("<f12> o t" . org-todo-list))
