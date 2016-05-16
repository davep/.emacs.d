;;; webinfo.el - Get information about a web server.
;; Copyright 2002 by Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

;; webinfo.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

(require 'cl)

;;;###autoload
(defun webinfo (host)
  (interactive "sHost: ")
  (with-output-to-temp-buffer "*webinfo*"
    (let ((s (open-network-stream (format "webinfo-%s" host) nil host "http")))
      (when s
        (unwind-protect
             (progn
               (set-process-filter s (lambda (o p) (princ (delete 13 p))))
               (process-send-string s "HEAD / HTTP/1.0\r\n\r\n")
               (while (eq (process-status s) 'open)
                 (sit-for 0.01)))
          (delete-process s))))))

(provide 'webinfo)

;;; webinfo.el ends here.
