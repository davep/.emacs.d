;;; become-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "become" "become.el" (22748 55062 0 0))
;;; Generated autoloads from become.el

(autoload 'become-dos-buffer "become" "\
Turn the current buffer into a DOS buffer.

\(fn)" t nil)

(autoload 'become-unix-buffer "become" "\
Turn the current buffer into a Unix buffer.

\(fn)" t nil)

(autoload 'become-mac-buffer "become" "\
Turn the current buffer into a Mac buffer.

\(fn)" t nil)

(autoload 'become-undosly "become" "\
Strip current buffer of DOS line end markers.

\(fn)" t nil)

(autoload 'become-freshly-indented "become" "\
Apply indentation to whole buffer.

\(fn)" t nil)

(autoload 'become-freshly-indented-no-tabs "become" "\
Apply indentation to whole buffer and then untabify.

\(fn)" t nil)

(autoload 'become-free-of-trailing-whitespace "become" "\
Remove all trailing whitespace from all lines in the current buffer.

Note that this function makes a point of not stripping the trailing space
from a signature seperator line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("become-pkg.el") (22746 24524 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; become-autoloads.el ends here
