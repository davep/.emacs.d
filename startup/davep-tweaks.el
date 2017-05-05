;;; davep-tweaks.el --- Miscellaneous "tweaks" to my emacs environment

;;; Commentary:
;;
;; davep-tweaks.el contains miscellaneous tweaks to the Emacs environment.
;; These are the sorts of changes that don't directly relate to packages,
;; etc, and aren't about how Emacs looks.

;;; Code:

;; On the Mac local bin doesn't seem to be in the path if I run from the
;; dock. Fix this.
(let ((local "/usr/local/bin"))
  (when (and is-a-macOS-p (not (member local exec-path)))
    (push local exec-path)))

;; If we're on a Unix of some sort, add a personal bin (if it's there).
(let ((bin "~/bin"))
  (when (and is-a-unixp (file-exists-p bin))
    (push bin exec-path)))

;; Enable some disabled commands
(mapc (lambda (command)
        (put command 'disabled nil))
      '(downcase-region
        erase-buffer
        narrow-to-page
        narrow-to-region
        upcase-region))

(provide 'davep-tweaks)

;;; davep-tweaks.el ends here
