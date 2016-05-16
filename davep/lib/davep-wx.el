;;; davep-wx.el - Display current weather at davep.org.
;; Copyright 2006-2008 by Dave Pearson <davep@davep.org>
;; $Revision: 1.2 $

;; davep-wx.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Code:

;; Things we need:
(require 'xml)
(require 'cl)

;; Customize options.

(defvar dpwx-current-host "wxw.davep.org"
  "*Host that holds the current weather data.")

(defvar dpwx-current-location "/xml/"
  "*Location of the current weather data.")

;; Main code.

(defun dpwx-get-current ()
  "Downlods the current weather from `dpwx-current'."
  (with-output-to-string
      (let ((s (open-network-stream (format "dpwx-%s" dpwx-current-host) nil dpwx-current-host "http")))
        (when s
          (unwind-protect
               (progn
                 (set-process-filter s (lambda (o p) (princ (delete 13 p))))
                 (process-send-string s (format "GET /%s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n" dpwx-current-location dpwx-current-host))
                 (while (eq (process-status s) 'open)
                   (sit-for 0.01)))
            (delete-process s))))))

(defun dpwx-read-current ()
  "Read the current weather readings for davep.org."
  (with-temp-buffer
    (insert (dpwx-get-current))
    (setf (point) (point-min))
    (when (search-forward "<?xml" nil t)
      (xml-parse-region (line-beginning-position) (point-max)))))

;;;###autoload
(defun dpwx ()
  "Display the current weather conditions at davep.org."
  (interactive)
  (flet ((fmt (item)
           (let ((units (cdr (assoc 'units (second item)))))
             (if units
                 (format "%s%s" (third item) units)
               (third item))))
         (rain (rain)
           (with-output-to-string
               (loop for value in (cddr rain)
                     do (when (listp value)
                          (princ (format "Rain %2s%s...: "
                                         (cdr (assoc 'period (cadr value)))
                                         (cdr (assoc 'time-unit (cadr value)))))
                          (princ (fmt value)) (terpri))))))
    (let ((wx (assoc 'conditions (car (dpwx-read-current)))))
      (with-output-to-temp-buffer "*Weather at davep.org*"
        (princ
         (format (concat "Time.......: %s\n"
                         "Temperature: %s\n"
                         "Dew Point..: %s\n"
                         "Humidity...: %s%%\n"
                         "Wind.......: %s from %s\n"
                         "%s"
                         "Pressure...: %s\n"
                         "Tendency...: %s\n"
                         "Forecast...: %s\n"
                         "Cloud Base.: %s")
                 (fmt (assoc 'when wx))
                 (fmt (assoc 'temperature wx))
                 (fmt (assoc 'dewpoint wx))
                 (fmt (assoc 'humidity wx))
                 (fmt (assoc 'speed (cdr (assoc 'wind wx))))
                 (fmt (assoc 'text (assoc 'direction (cdr (assoc 'wind wx)))))
                 (rain (assoc 'rain wx))
                 (fmt (assoc 'adjusted (assoc 'pressure wx)))
                 (fmt (assoc 'tendency (assoc 'pressure wx)))
                 (fmt (assoc 'forecast wx))
                 (fmt (assoc 'estcloudbase wx))))))))

;;; davep-wx.el ends here
