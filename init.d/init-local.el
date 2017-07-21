;;; init-local.el --- Set up local config storage.

;;; Commentary:
;;
;; init-local.el takes care of setting up the location of the "local"
;; configs and storage. As much as possible I try and keep things out of
;; ~/.emacs.d/ if I'm not going to sync them amongst my machines.

;;; Code:

(defconst local-emacs-directory "~/.local/share/emacs/"
  "Directory where local Emacs content lives.

This is the directory where files I don't want to sync via
github, and which I don't want to lose if I erase my ~/.emacs.d/
directory, should live.")

(defun local-emacs-directory (content)
  "Return the local Emacs directory path for CONTENT.

A side-effect of calling this function is that it checks if the
directory pointed to by the variable `local-emacs-directory'
exists and, if it doesn't, it creates it."
  (unless (file-exists-p local-emacs-directory)
    (make-directory local-emacs-directory t))
  (expand-file-name content local-emacs-directory))

(provide 'init-local)

;;; init-local.el ends here
