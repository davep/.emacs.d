;;; moving.el --- Extra commands for moving about a buffer.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

(eval-when-compile
  (require 'cl))

;;;###autoload
(defun next-repeated-word ()
  "Find the the next repeated word."
  (interactive)
  (search-forward-regexp "\\(\\<\\w+\\>\\)\\(\\s-\\|\t\\|\n\\)+\\<\\1\\>"))

;;;###autoload
(defun smart-home ()
  "Move `point' towards \"home\" depending on context."
  (interactive)
  (cond ((bolp)
         (setf (point) (point-min)))
        ((<= (point) (save-excursion (back-to-indentation) (point)))
         (beginning-of-line))
        (t
         (back-to-indentation))))

;;;###autoload
(defun smart-end ()
  "Move `point' towards \"end\" depending on context."
  (interactive)
  (cond ((eolp)
         (setf (point) (point-max)))
        ((< (point) (save-excursion (back-to-indentation) (point)))
         (back-to-indentation))
        (t
         (end-of-line))))

(provide 'moving)

;; moving.el ends here
