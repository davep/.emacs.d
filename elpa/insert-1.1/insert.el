;;; insert.el --- Insert stuff.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: convenience
;; URL: https://github.com/davep/insert.el

;;; Commentary:
;;
;; insert.el contains commands for quickly and easily inserting useful
;; things into the current buffer.

;;; Code:

(require 'thingatpt)

;;;###autoload
(defun insert-filename (file)
  "Insert a name of FILE allowing for interactive browsing to the name."
  (interactive "fFile: ")
  (insert file))

;;;###autoload
(defun insert-buffer-filename (&optional name-only)
  "Insert the filename of the current buffer.

NAME-ONLY is a prefix argument, nil means insert the full name of the file,
any other value means insert the name without the directory."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (if (null filename)
        (error "Buffer has no filename")
      (insert (if name-only
                  (file-name-nondirectory filename)
                filename)))))

;;;###autoload
(defun insert-sexp-link ()
  "Place \"link quotes\" around the `sexp-at-point'."
  (interactive)
  (when (sexp-at-point)
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (save-excursion
        (setf (point) (car bounds))
        (insert "`")
        (setf (point) (1+ (cdr bounds)))
        (insert "'")))))

;;;###autoload
(defun insert-snip (start end)
  "Call `kill-region' on region bounding START and END and then insert \"[SNIP]\"."
  (interactive "r")
  (kill-region start end)
  (insert "[SNIP]")
  (forward-char -1))

;;;###autoload
(defun insert-tags (tag start end)
  "Surround region bounded by START and END with xml/sgml/html tag TAG."
  (interactive "sTag: \nr")
  (let ((text (buffer-substring start end)))
    (setf (buffer-substring start end)
          (concat "<" tag ">" text "</" tag ">"))))

;;;###autoload
(defun insert-line-split-keeping-fill-prefix ()
  "Like `split-line' but trys to keep the `fill-prefix'.

Also adds an extra blank line to the split because it's mostly
intended for use with editing quoted text."
  (interactive)
  (let* ((fill-prefix (fill-context-prefix (save-excursion
                                             (backward-paragraph)
                                             (point))
                                           (save-excursion
                                             (forward-paragraph)
                                             (point))))
         (spaces (- (point)
                    (line-beginning-position)
                    (length fill-prefix))))
    (if (wholenump spaces)
        (save-excursion
          (insert (format "\n\n%s%s" fill-prefix (make-string spaces ? ))))
      (error "Can't split within the fill prefix"))))

;;;###autoload
(defun insert-cut-here (&optional say-cut)
  "Insert \"cut here\" delimeters."
  (interactive "P")
  (let ((cut-line (if say-cut
                      (concat "-- cut here " (make-string 64 ?-))
                    (make-string 76 ?-))))
    (insert (format "%s\n%s\n" cut-line cut-line))
    (forward-line -1)))

;;;###autoload
(defun insert-file-cut-here (file)
  "Insert a file with a \"cut here\" delimiter."
  (interactive "fFilename: ")
  (insert-cut-here t)
  (insert-file-contents-literally file))

(defconst insert--melpa-badge-types
  `((markdown .
              ,(concat
                "[![MELPA Stable](https://stable.melpa.org/packages/{{p}}-badge.svg)](https://stable.melpa.org/#/{{p}})"
                "\n"
                "[![MELPA](https://melpa.org/packages/{{p}}-badge.svg)](https://melpa.org/#/{{p}})"))
    (html .
          ,(concat
            "<a href=\"https://stable.melpa.org/#/{{p}}\"><img alt=\"MELPA Stable\" src=\"https://stable.melpa.org/packages/{{p}}-badge.svg\"/></a>"
            "\n"
            "<a href=\"https://melpa.org/#/{{p}}\"><img alt=\"MELPA\" src=\"https://melpa.org/packages/{{p}}-badge.svg\"/></a>")))
  "Types of output for `insert-melpa-badge'.")

;;;###autoload
(defun insert-melpa-badge (package type)
  "Insert melpa badge code, for document TYPE, for PACKAGE."
  (interactive
   (list
    (read-file-name "Package: ")
    (completing-read "Type: " insert--melpa-badge-types)))
  (let ((fmt (cdr (assoc (intern type) insert--melpa-badge-types))))
    (when fmt
      (insert (replace-regexp-in-string "{{p}}" (file-name-nondirectory (file-name-sans-extension package)) fmt)))))

(provide 'insert)

;;; insert.el ends here
