;;; itch.el --- Scratch buffer tools.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience
;; URL: https://github.com/davep/itch.el

;;; Commentary:
;;
;; itch.el provides a tool for quickly and easily getting back to your
;; *scratch* buffer (with optional erase).

;;; Code:

;;;###autoload
(defun itch-scratch-buffer (erase)
  "Quickly switch to the *scratch* buffer.

If ERASE is non-nil reset the content of the buffer."
  (interactive "P")
  (switch-to-buffer "*scratch*")
  (when erase
    (erase-buffer)
    (insert initial-scratch-message))
  (lisp-interaction-mode))

(provide 'itch)

;;; itch.el ends here
