;;; astro-me-uk.el --- Tools for managing my astronomy web site.
;; Copyright 2005 by Dave Pearson <davep@davep.org>
;; $Revision$

(eval-when-compile
  (require 'cl))

(defvar astro-me-uk-root "~/html/www.astronomer.me.uk/"
  "Root directory of www.astronomer.me.uk sources.")

(defun astro-me-uk-path (path)
  "Return PATH within `astro-me-uk-root'."
  (format "%s/%s" astro-me-uk-root path))

;;;###autoload
(defun astro-me-uk-changelog ()
  "Add an entry to the www.astronomer.me.uk ChangeLog."
  (interactive)
  (let ((add-log-time-format 'add-log-iso8601-time-string))
    (with-temp-buffer
      (add-change-log-entry nil (astro-me-uk-path "site/ChangeLog/ChangeLog")))))


(provide 'astro-me-uk)

;;; astro-me-uk.el ends here.
