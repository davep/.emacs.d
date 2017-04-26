;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On the Mac local bin doesn't seem to be in the path if I run from
;; the dock. Fix this.
(let ((local "/usr/local/bin"))
  (when (and davep:macOS-p (not (member local exec-path)))
    (push local exec-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If we're on a Unix of some sort, add a personal bin (if it's there).
(when (and davep:unixp (file-exists-p "~/bin"))
  (push "~/bin/" exec-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable some disabled commands
(mapc (lambda (command)
        (put command 'disabled nil))
      '(downcase-region
        erase-buffer
        narrow-to-page
        narrow-to-region
        upcase-region))

(provide 'davep-tweaks)
