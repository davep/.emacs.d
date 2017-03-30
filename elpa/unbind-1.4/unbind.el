;;; unbind.el --- Commands for unbinding things
;; Copyright 2002-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.4
;; Keywords: lisp, unbind
;; URL: https://github.com/davep/unbind.el
;; Package-Requires: ((cl-lib "0.5"))

;; unbind.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; unbind.el provides some simple commands for unbinding values in Emacs
;; Lisp. I find this handy when working on code.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun unbind-defun ()
  "Unbind the `defun' near `point' in `current-buffer'."
  (interactive)
  (save-excursion
    (if (and (beginning-of-defun) (looking-at "(defun"))
        (fmakunbound (cadr (read (current-buffer))))
      (error "No defun found near point"))))

;;;###autoload
(defun unbind-symbol (symbol)
  "Totally unbind SYMBOL.

This includes unbinding its function binding, its variable binding and its
property list."
  (interactive "SSymbol: ")
  (fmakunbound symbol)
  (makunbound symbol)
  (setf (symbol-plist symbol) nil))

;;;###autoload
(defun unbind-function (symbol)
  "Remove the function binding of SYMBOL."
  (interactive "aFunction: ")
  (fmakunbound symbol))

;;;###autoload
(defun unbind-command (symbol)
  "Remove the command binding of SYMBOL."
  (interactive "CCommand: ")
  (fmakunbound symbol))

;;;###autoload
(defun unbind-variable (symbol)
  "Remove the variable binding of SYMBOL."
  (interactive (list (completing-read "Variable: "
                                      (cl-loop for s being the symbols
                                            when (boundp s) collect (list (symbol-name s))))))
  (makunbound (if (stringp symbol) (intern symbol) symbol)))

(provide 'unbind)

;;; unbind.el ends here
