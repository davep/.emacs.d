;;; csrclr.el --- Set cursor colour depending on environment.
;; Copyright 2000-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: convenience
;; URL: https://github.com/davep/csrclr.el

;;; Commentary:
;;
;; csrclr.el provides a command that sets the cursor to a different colour
;; depending on the environment. I find this especially handy if I open a
;; read-only buffer.

;;; Code:

(defgroup csrclr nil
  "Cursor colour."
  :group  'editing
  :prefix "csrclr-")

(defcustom csrclr-read-only "red"
  "Colour of the cursor when the buffer is read only."
  :type  'string
  :group 'csrclr)

(defcustom csrclr-overwrite "pink"
  "Colour of the cursor when in overwrite mode."
  :type  'string
  :group 'csrclr)

(defcustom csrclr-default "black"
  "Default cursor colour."
  :type  'string
  :group 'csrclr)

;; Main code:

(defun csrclr ()
  "Set the cursor colour depending on the environment."
  (set-cursor-color
   (cond ((and buffer-read-only (buffer-file-name))
          csrclr-read-only)
         (overwrite-mode
          csrclr-overwrite)
         (t
          csrclr-default))))

(add-hook 'post-command-hook #'csrclr)

(provide 'csrclr)

;;; csrclr.el ends here
