(defvar speechd-device "/dev/speech")

(defun speechd-region (start end)
  "Speak the text in region."
  (interactive "r")
  (write-region start end speechd-device))

(defun speechd-buffer (buffer)
  "Read the content of a buffer."
  (interactive "b")
  (with-current-buffer buffer
    (speechd-region (point-min) (point-max))))

(defun speechd-current-buffer ()
  "Read the current buffer."
  (interactive)
  (speechd-buffer (current-buffer)))
