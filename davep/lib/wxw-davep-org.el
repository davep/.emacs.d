;;; wxw-davep-org.el --- Tools for managing my weather web site.
;; Copyright 2005-2017 by Dave Pearson <davep@davep.org>

(eval-when-compile
  (require 'cl))

(defvar wxw-davep-org-root "~/Sites/wxw.davep.org/"
  "Root directory of wxw.davep.org sources.")

(defun wxw-davep-org-path (path)
  "Return PATH within `wxw-davep-org-root'."
  (format "%s/%s" wxw-davep-org-root path))

;;;###autoload
(defun wxw-davep-org-changelog ()
  "Add an entry to the wxw.davep.org ChangeLog."
  (interactive)
  (let ((add-log-time-format 'add-log-iso8601-time-string))
    (with-temp-buffer
      (add-change-log-entry nil (wxw-davep-org-path "site/ChangeLog/ChangeLog")))))

(provide 'wxw-davep-org)

;;; wxw-davep-org.el ends here.
