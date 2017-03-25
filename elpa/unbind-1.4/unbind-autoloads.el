;;; unbind-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "unbind" "unbind.el" (22742 29796 0 0))
;;; Generated autoloads from unbind.el

(autoload 'unbind-defun "unbind" "\
Unbind the `defun' near `point' in `current-buffer'.

\(fn)" t nil)

(autoload 'unbind-symbol "unbind" "\
Totally unbind SYMBOL.

This includes unbinding its function binding, its variable binding and its
property list.

\(fn SYMBOL)" t nil)

(autoload 'unbind-function "unbind" "\
Remove the function binding of SYMBOL.

\(fn SYMBOL)" t nil)

(autoload 'unbind-command "unbind" "\
Remove the command binding of SYMBOL.

\(fn SYMBOL)" t nil)

(autoload 'unbind-variable "unbind" "\
Remove the variable binding of SYMBOL.

\(fn SYMBOL)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; unbind-autoloads.el ends here
