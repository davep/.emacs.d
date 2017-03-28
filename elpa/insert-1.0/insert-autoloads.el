;;; insert-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "insert" "insert.el" (22746 18056 0 0))
;;; Generated autoloads from insert.el

(autoload 'insert-filename "insert" "\
Insert a name of FILE allowing for interactive browsing to the name.

\(fn FILE)" t nil)

(autoload 'insert-buffer-filename "insert" "\
Insert the filename of the current buffer.

NAME-ONLY is a prefix argument, nil means insert the full name of the file,
any other value means insert the name without the directory.

\(fn &optional NAME-ONLY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; insert-autoloads.el ends here
