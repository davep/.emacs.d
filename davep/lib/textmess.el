;;; textmess.el --- Commands and functions for messing with text.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.8 $

(eval-when-compile
  (require 'cl))

;;;###autoload
(defun strip-trailing-whitespace ()
  "Remove all trailing whitespace from all lines.

Note that this function makes a point of not stripping the trailing space
from a signature seperator line."
  (interactive)
  (flet ((is-sig-line ()
           (save-excursion
             (beginning-of-line)
             (looking-at "^-- $"))))
    (save-excursion
      (setf (point) (point-min))
      (while (re-search-forward "[ \t\r]+$" nil t)
        (unless (is-sig-line)
          (replace-match "" nil nil))))))

;;;###autoload
(defun indent-buffer ()
  "Apply indentation to whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;;###autoload
(defun indent-buffer-and-untabify ()
  "Apply indentation to whole buffer and then untabify."
  (interactive)
  (indent-buffer)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun sexp-link ()
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
(defun cut-here (&optional say-cut)
  "Insert \"cut here\" delimeters."
  (interactive "P")
  (let ((cut-line (if say-cut
                      (concat "-- cut here " (make-string 64 ?-))
                    (make-string 76 ?-))))
    (insert (format "%s\n%s\n" cut-line cut-line))
    (forward-line -1)))

;;;###autoload
(defun cut-file-here (file)
  "Insert a file with a \"cut here\" delimiter."
  (interactive "fFilename: ")
  (cut-here t)
  (insert-file-contents-literally file))

;;;###autoload
(defun snip (start end)
  "Call `kill-region' and then insert \"[SNIP]\""
  (interactive "r")
  (kill-region start end)
  (insert "[SNIP]")
  (forward-char -1))

;;;###autoload
(defun split-line-keeping-fill-prefix ()
  "Like `split-line' but trys to keep the `fill-prefix'."
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
(defun tagify (tag start end)
  "Surround region bounded by START and END with xml/sgml/html tag TAG."
  (interactive "sTag: \nr")
  (let ((text (buffer-substring start end)))
    (setf (buffer-substring start end)
          (concat "<" tag ">" text "</" tag ">"))))

(provide 'textmess)

;; textmess.el ends here
