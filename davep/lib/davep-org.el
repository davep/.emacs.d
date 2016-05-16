;;; davep-org.el --- Tools for managing my web site.
;; Copyright 2001-2005 by Dave Pearson <davep@davep.org>
;; $Revision: 1.9 $

(eval-when-compile
  (require 'cl))

(defvar davep-org-root "~/html/www.davep.org/"
  "Root directory of www.davep.org sources.")

(defun davep-org-path (path)
  "Return PATH within `davep-org-root'."
  (format "%s/%s" davep-org-root path))

;;;###autoload
(defun davep-org-changelog ()
  "Add an entry to the www.davep.org ChangeLog."
  (interactive)
  (let ((add-log-time-format 'add-log-iso8601-time-string))
    (with-temp-buffer
      (add-change-log-entry nil (davep-org-path "site/ChangeLog/ChangeLog")))))

(provide 'davep-org)

;;; davep-org.el ends here.
