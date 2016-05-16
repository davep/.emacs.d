;;; nukneval.el - Nuke and reevaluate an elisp buffer.
;; Copyright 2002 by Dave Pearson <davep@davep.org>
;; $Revision$

;; nukneval.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;; Things we need:
(eval-when-compile
  (require 'cl))

;;;###autoload
(defun nuke-and-eval ()
  "Attempt to cleanly reevaluate a buffer of elisp code."
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (loop for form = (condition-case nil
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

;;; nukneval.el ends here.
