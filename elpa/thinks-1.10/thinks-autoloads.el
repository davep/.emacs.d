;;; thinks-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "thinks" "thinks.el" (22742 21899 0 0))
;;; Generated autoloads from thinks.el

(autoload 'thinks "thinks" "\
Insert TEXT wrapped in a think bubble.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you.

\(fn TEXT)" t nil)

(autoload 'thinks-region "thinks" "\
Bubble wrap region bounding START and END.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you.

\(fn START END)" t nil)

(autoload 'thinks-yank "thinks" "\
Do a `yank' and bubble wrap the yanked text.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you.

\(fn)" t nil)

(autoload 'thinks-maybe-region "thinks" "\
If region is active, bubble wrap region bounding START and END.
If not, query for text to insert in bubble.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; thinks-autoloads.el ends here
