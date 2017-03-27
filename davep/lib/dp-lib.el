;;; Collection of handy functions for emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find a file and insert its name into the buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-filename (file)
  "davep: Insert a filename allowing for interactive browsing to the name"
  (interactive "fFile: ")
  (insert file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert the current buffer's filename into the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-buffer-filename (&optional name-only)
  "davep: Insert the filename of the current buffer.

NAME-ONLY is a prefix argument, nil means insert the full name of the file,
any other value means insert the name without the directory."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (if (null filename)
        (error "Buffer has no filename")
      (insert (if name-only
                  (file-name-nondirectory filename)
                filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for messing with codings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun become-dos-buffer ()
  "Turn the current buffer into a DOS buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun become-unix-buffer ()
  "Turn the current buffer into a Unix buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(provide 'dp-lib)
