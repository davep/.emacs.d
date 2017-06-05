;;; init-tweaks.el --- Miscellaneous "tweaks" to my emacs environment

;;; Commentary:
;;
;; init-tweaks.el contains miscellaneous tweaks to the Emacs environment.
;; These are the sorts of changes that don't directly relate to packages,
;; etc, and aren't about how Emacs looks.

;;; Code:

(require 'is-a)

;; On the Mac, local bin doesn't seem to be in the path if I run from the
;; dock. Fix this.
(let ((local "/usr/local/bin"))
  (when (and is-a-macOS-p (not (member local exec-path)))
    (push local exec-path)))

;; If we're on a Unix of some sort, add a personal bin (if it's there).
(let ((bin "~/bin"))
  (when (and is-a-unix-p (file-exists-p bin))
    (push bin exec-path)))

;; Try and get aspell working on a Windows machine.
(let ((aspell "C:/Program Files (x86)/Aspell/bin/"))
  (when (and is-a-win32-p (file-exists-p aspell))
    (push aspell exec-path)
    (setq ispell-program-name "aspell")))

;; Enable some disabled commands
(mapc (lambda (command)
        (put command 'disabled nil))
      '(downcase-region
        erase-buffer
        narrow-to-page
        narrow-to-region
        upcase-region))

;; Some functions in elisp seem to be missing sensible indent declare forms.
;; Let's fix them.
(mapc (lambda (indent)
        (put (car indent) 'lisp-indent-function (cdr indent)))
      '((with-current-buffer-window . 3)
        (with-temp-buffer-window    . 3)))

(provide 'init-tweaks)

;;; init-tweaks.el ends here
