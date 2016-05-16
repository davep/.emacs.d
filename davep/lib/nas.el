;;; nas.el --- Tools for managing the Newton's Astronomical Society website.
;; Copyright 2005 by Dave Pearson <davep@davep.org>
;; $Revision: 1.3 $

(eval-when-compile
  (require 'cl))

(defvar nas-root "~/html/www.newtonsas.org.uk/"
  "Root directory of www.newtonsas.org.uk sources.")

(defvar nas-news-items nil
  "Cache of news items.")

(defun nas-path (path)
  "Return PATH within `nas-root'."
  (format "%s/%s" nas-root path))

;;;###autoload
(defun nas-nas ()
  "Insert some HTML for NAS as an acronym."
  (interactive)
  (insert "<acronym title=\"Newton's Astronomical Society\">NAS</acronym>"))

;;;###autoload
(defun nas-changelog ()
  "Add an entry to the www.newtonsas.org.uk ChangeLog."
  (interactive)
  (let ((add-log-time-format 'add-log-iso8601-time-string))
    (with-temp-buffer
      (add-change-log-entry nil (nas-path "ChangeLog/ChangeLog")))))

(defun nas-news-time (time)
  "Return a news item time stamp."
  (format-time-string "%Y-%m-%d %H:%M:%S <acronym title=\"Greenwich Mean Time\">GMT</acronym>" time t))

;;;###autoload
(defun nas-news (title)
  "Create a new news item."
  (interactive "sTitle: ")
  (let ((time (current-time)))
    (find-file (format-time-string (nas-path "news/%Y%m%d%H%M%S.txt") time t))
    (flyspell-mode)
    (setf (buffer-string) (format "%s\n%s\n\n" title (nas-news-time time)))))

(defun nas-get-news ()
  "Load the list of news items."
  (setq nas-news-items
        (loop for item in (directory-files (nas-path "news/") nil "^[0-9]+\.txt$")
              collect (cons (with-temp-buffer
                              (insert-file-literally (concat (nas-path "news/") item))
                              (buffer-substring-no-properties (point-min) (line-end-position)))
                            (file-name-sans-extension item)))))

(defun nas-prompt-for-news ()
  "Interactive prompt for getting at news items."
  (let ((completion-ignore-case t))
    (completing-read "News Item: " (nas-get-news) nil t)))

;;;###autoload
(defun nas-insert-news-id (item)
  "Insert the ID of the news item whose title is ITEM."
  (interactive (list (nas-prompt-for-news)))
  (insert (cdr (assoc item nas-news-items))))

;;;###autoload
(defun nas-edit-news (item)
  "Edit the news item whose title is ITEM."
  (interactive (list (nas-prompt-for-news)))
  (find-file (format (nas-path "news/%s.txt") (or (cdr (assoc item nas-news-items)) (error "No such news item"))))
  (flyspell-mode))

;;;###autoload
(defun nas-update-news (item)
  "Update the news item whose title is ITEM."
  (interactive (list (nas-prompt-for-news)))
  (nas-edit-news item)
  (setf (point) (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert (format "<p>\n<span class=\"minor\">%s</span>\n<p>\n" (nas-news-time (current-time))))
  (not-modified))

(provide 'nas)

;;; nas.el ends here.
