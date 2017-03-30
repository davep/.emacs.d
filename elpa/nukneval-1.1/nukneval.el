;;; nukneval.el --- Nuke and reevaluate an elisp buffer.
;; Copyright 2002-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: lisp
;; URL: https://github.com/davep/nukneval.el
;; Package-Requires: ((cl-lib "0.5"))

;; nukneval.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;; Things we need:

;;; Commentary:
;;
;; nukneval.el provides a command that attempts to cleanly reevaluate a
;; buffer of elisp code.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun nuke-and-eval ()
  "Attempt to cleanly reevaluate a buffer of elisp code."
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (cl-loop for form = (condition-case nil
                            (read (current-buffer))
                          (error nil))
       while form
       do (let ((type (car form))
                (name (cadr form)))
            (cond
              ((memq type '(defun defun* defsubst defalias defmacro))
               (fmakunbound name))
              ((memq type '(defvar defparameter defconst defcustom))
               (makunbound name))))))
  (eval-buffer))

(provide 'nukneval)

;;; nukneval.el ends here
