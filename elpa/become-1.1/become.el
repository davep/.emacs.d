;;; become.el --- Tools for transforming a buffer.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: convenience
;; URL: https://github.com/davep/become.el
;; Package-Requires: ((cl-lib "0.5"))

;;; Commentary:
;;
;; become.el provides a set of interactive functions that allow for easy
;; transformation of the current buffer.

;;; Code:

;; Things we need:
(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun become-dos-buffer ()
  "Turn the current buffer into a DOS buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

;;;###autoload
(defun become-unix-buffer ()
  "Turn the current buffer into a Unix buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

;;;###autoload
(defun become-mac-buffer ()
  "Turn the current buffer into a Mac buffer."
  (interactive)
  (set-buffer-file-coding-system 'mac))

;;;###autoload
(defun become-undosly ()
  "Strip current buffer of DOS line end markers."
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (while (search-forward "\015" nil t)
      (replace-match "" nil nil))
    (setf (point) (point-min))
    (while (search-forward "\032" nil t)
      (replace-match "" nil nil))
    (when (called-interactively-p 'interactive)
      (message "Buffer is now sane"))))

;;;###autoload
(defun become-freshly-indented ()
  "Apply indentation to whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;;###autoload
(defun become-freshly-indented-no-tabs ()
  "Apply indentation to whole buffer and then untabify."
  (interactive)
  (indent-buffer)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun become-free-of-trailing-whitespace ()
  "Remove all trailing whitespace from all lines in the current buffer.

Note that this function makes a point of not stripping the trailing space
from a signature seperator line."
  (interactive)
  (cl-flet ((is-sig-line ()
              (save-excursion
                (beginning-of-line)
                (looking-at "^-- $"))))
    (save-excursion
      (setf (point) (point-min))
      (while (re-search-forward "[ \t\r]+$" nil t)
        (unless (is-sig-line)
          (replace-match "" nil nil))))))

(provide 'become)

;;; become.el ends here
