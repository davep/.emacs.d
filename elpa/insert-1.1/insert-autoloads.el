;;; insert-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "insert" "insert.el" (22747 45905 0 0))
;;; Generated autoloads from insert.el

(autoload 'insert-filename "insert" "\
Insert a name of FILE allowing for interactive browsing to the name.

\(fn FILE)" t nil)

(autoload 'insert-buffer-filename "insert" "\
Insert the filename of the current buffer.

NAME-ONLY is a prefix argument, nil means insert the full name of the file,
any other value means insert the name without the directory.

\(fn &optional NAME-ONLY)" t nil)

(autoload 'insert-sexp-link "insert" "\
Place \"link quotes\" around the `sexp-at-point'.

\(fn)" t nil)

(autoload 'insert-snip "insert" "\
Call `kill-region' on region bounding START and END and then insert \"[SNIP]\".

\(fn START END)" t nil)

(autoload 'insert-tags "insert" "\
Surround region bounded by START and END with xml/sgml/html tag TAG.

\(fn TAG START END)" t nil)

(autoload 'insert-line-split-keeping-fill-prefix "insert" "\
Like `split-line' but trys to keep the `fill-prefix'.

Also adds an extra blank line to the split because it's mostly
intended for use with editing quoted text.

\(fn)" t nil)

(autoload 'insert-cut-here "insert" "\
Insert \"cut here\" delimeters.

\(fn &optional SAY-CUT)" t nil)

(autoload 'insert-file-cut-here "insert" "\
Insert a file with a \"cut here\" delimiter.

\(fn FILE)" t nil)

(autoload 'insert-melpa-badge "insert" "\
Insert melpa badge code, for document TYPE, for PACKAGE.

\(fn PACKAGE TYPE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; insert-autoloads.el ends here
