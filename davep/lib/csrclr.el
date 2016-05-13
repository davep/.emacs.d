;;; csrclr.el --- Set cursor colour depending on environment.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.2 $

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

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
  (set-cursor-color (cond ((and buffer-read-only (buffer-file-name))
                           csrclr-read-only)
                          (overwrite-mode
                           csrclr-overwrite)
                          (t
                           csrclr-default))))

(add-hook 'post-command-hook #'csrclr)

(provide 'csrclr)

;;; csrclr.el ends here
