;;; handyurl.el --- Pop up a list of URLs and select for pasting.
;; Copyright 1998-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 2.0
;; Keywords: convenience, quoting
;; URL: https://github.com/davep/handyurl.el
;; Package-Requires: ((cl-lib "0.5"))

;; handyurl.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; This package provides a simple method of popping up a list of URLs and
;; allowing the selection and insertion of an URL into the previous buffer.
;;
;; The URLs are stored in an external file as a list of cons cells, for example:
;;
;; (( "The GNU Project" . "http://www.gnu.org/")
;;  ( "The FSF"         . "http://www.fsf.org/"))
;;
;; The name and location of the file is up to you, the default name used by
;; the function `handyurl' is stored in `handyurl-file'.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; Customize options.

(defgroup handyurl nil
  "Insert an URL from a list of URLs."
  :group 'convenience
  :prefix "handyurl-")

(defcustom handyurl-file (locate-user-emacs-file ".handyurls.el" "~/.handyurls")
  "*Name of file from which `handyurl' should read the URLs."
  :type  'file
  :group 'handyurl)

(defcustom handyurl-mode-hook nil
  "*Hooks for `handyurl-mode'."
  :type  'hook
  :group 'handyurl)

(defcustom handyurl-sort-predicate #'(lambda (first second)
                                       (string< (upcase (car first))
                                                (upcase (car second))))
  "*Predicate for sorting the URLs before display.

Setting this variable to NIL means \"don't sort\"."
  :type  'function
  :group 'handyurl)

(defvar handyurl-urls nil
  "Contains the list of URL details.")

(defvar handyurl-last-buffer nil
  "Pointer to the calling buffer.")

(defvar handyurl-mode-map nil
  "Local keymap for the `handyurl' buffer.")

(defvar handyurl-buffer-name "*Handyurl*"
  "Name for the URL listing buffer.")

(unless handyurl-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [(control m)] #'handyurl-insert-url)
    (define-key map "u"           #'handyurl-insert-naked-url)
    (define-key map " "           #'handyurl-insert-url-with-name)
    (define-key map "n"           #'handyurl-insert-name)
    (define-key map [(control g)] #'handyurl-select-quit)
    (define-key map "q"           #'handyurl-select-quit)
    (define-key map "?"           #'describe-mode)
    (setq handyurl-mode-map map)))

(put 'handyurl-mode 'mode-class 'special)

(defun handyurl-mode ()
  "A mode for use with `handyurl'.

The key bindings for `handyurl-mode' are:

\\{handyurl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map handyurl-mode-map)
  (setq major-mode 'handyurl-mode
	mode-name  "Handy URL")
  (run-hooks 'handyurl-mode-hook)
  (setq buffer-read-only t))

;;;###autoload
(cl-defun handyurl (&optional (url-file handyurl-file))
  "Pick an URL from a list of URLs and paste it into the current buffer.
URL-FILE is the name of the file to read the URL list from, if not supplied
the file pointed to by `handyurl-file' is read."
  (interactive)
  (unless (string= (buffer-name) handyurl-buffer-name)
    (setq handyurl-last-buffer (current-buffer)))
  (if (file-exists-p url-file)
      (progn
	(pop-to-buffer "*Handyurl*")
        (let ((buffer-read-only nil))
          (setf (buffer-string) "")
          (handyurl-read-urls url-file)
          (handyurl-display-urls)
          (setf (point) (point-min)))
        (handyurl-mode))
    (error "No such URL file '%s'" url-file)))

(defun handyurl-read-urls (url-file)
  "Read the URLS into the variable `handyurl-urls'.

Argument URL-FILE is the name of the file to read."
  (with-temp-buffer
    (insert-file-contents url-file t)
    (setq handyurl-urls (read (current-buffer))))
  (when handyurl-sort-predicate
    (setq handyurl-urls (sort handyurl-urls handyurl-sort-predicate))))

(defun handyurl-display-urls ()
  "Display the contans of `handyurl-urls' in the current buffer."
  (let ((fmt (format "%%-%ds - %%s\n"
                     (apply #'max (loop for url in handyurl-urls
                                        collect (length (car url)))))))
    (loop for url in handyurl-urls
          do (insert (format fmt (car url) (cdr url))))))

(defun handyurl-current-line ()
  "Work out the current line number."
  (save-excursion
    (beginning-of-line)
    (let ((line-point (point)))
      (setf (point) (point-min))
      (loop while (< (point) line-point) sum 1 do (forward-line 1)))))

(defun handyurl-insert (type)
  "Paste the url under the cursor to the current buffer.

TYPE specifies the kind of formatting to apply."
  (let ((url (nth (handyurl-current-line) handyurl-urls)))
    (if url
	(with-current-buffer handyurl-last-buffer
	  (insert (case type
                    ('url       (format "<URL:%s>" (cdr url)))
                    ('naked-url (cdr url))
                    ('with-name (format "%s <URL:%s>" (car url) (cdr url)))
                    ('name      (car url)))))
      (error "No URL details on that line"))
    url))

(defun handyurl-insert-url ()
  "Insert the selected URL into the buffer."
  (interactive)
  (when (handyurl-insert 'url)
    (handyurl-select-quit)))

(defun handyurl-insert-naked-url ()
  "Insert the selected URL into the buffer (with no formatting)."
  (interactive)
  (when (handyurl-insert 'naked-url)
    (handyurl-select-quit)))

(defun handyurl-insert-url-with-name ()
  "Insert the site name and the URL."
  (interactive)
  (when (handyurl-insert 'with-name)
    (handyurl-select-quit)))

(defun handyurl-insert-name ()
  "Insert the name of the site."
  (interactive)
  (when (handyurl-insert 'name)
    (handyurl-select-quit)))

(defun handyurl-select-quit ()
  "Kill the Handyurl frame."
  (interactive)
  (kill-buffer handyurl-buffer-name)
  (switch-to-buffer handyurl-last-buffer)
  (delete-other-windows))

(provide 'handyurl)

;;; handyurl.el ends here
