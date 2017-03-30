;;; webinfo.el --- Get information about a web server.
;; Copyright 2002 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: comm, net, http, web
;; URL: https://github.com/davep/webinfo.el

;; webinfo.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; Provides a simple command for getting information about a web server at a
;; given host, on a given port.

;;; Code:

;;;###autoload
(defun webinfo (host port)
  "Get some information about HTTP server at HOST on PORT."
  (interactive
   (list
    (read-string "Host: " '("localhost" . 0))
    (read-string "Port: " '("80" . 0))))
  (with-output-to-temp-buffer "*webinfo*"
    (let ((s (open-network-stream (format "webinfo-%s" host) nil host port)))
      (when s
        (unwind-protect
             (progn
               (set-process-filter s (lambda (o p) (princ (delete 13 p))))
               (process-send-string s "HEAD / HTTP/1.0\r\n\r\n")
               (while (eq (process-status s) 'open)
                 (sit-for 0.01)))
          (delete-process s))))))

(provide 'webinfo)

;;; webinfo.el ends here
