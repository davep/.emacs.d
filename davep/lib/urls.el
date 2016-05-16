;;; urls.el --- URL oriented commands.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.2 $

(eval-when-compile
  (require 'sawfish)
  (require 'boxquote))

(defun format-url (url)
  "Format URL with a <URL:...> wrapper."
  (format "<URL:%s>" url))

;;;###autoload
(defun insert-url (url)
  "davep: Insert an url with a <URL:...> wrapper."
  (interactive "sURL: ")
  (insert (format-url url)))

;;;###autoload
(defun insert-navigator-url (&optional no-formatting)
  "Insert the URL that Netscape Navigator is looking at.

Note that the URL of the first Navigator window found is inserted.

Also note that if `point' is within a boxquote then instead of inserting the
URL the URL is set as the title of the box."
  (interactive "P")
  (require 'sawfish)
  (require 'boxquote)
  (let ((url (sawfish-code (navigator-url))))
    (when url
      (let ((url (with-temp-buffer
                   (setf (buffer-string) url)
                   (setf (point) (point-min))
                   (replace-string "://hagbard/" "://")
                   (buffer-string))))
        (cond (no-formatting
               (insert url))
              ((boxquote-points)
               (boxquote-title (format-url url)))
              (t
               (insert-url url)))))))

;;;###autoload
(defun browse-buffer ()
  "View the current buffer in a web browser."
  (interactive)
  (if (buffer-file-name)
      (browse-url (concat "file:" (buffer-file-name)))
    (error "There is no file associated with this buffer")))

(provide 'urls)

;; urls.el ends here
