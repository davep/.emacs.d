;;; equinox.el - Code for working out the next equinox/solstice.
;; Copyright 2003 by Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

;; equinox.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

(eval-when-compile
  (require 'cl))
(require 'calendar)
(require 'solar)

(defun* find-next-equinox/solstice (&optional (date (calendar-current-date)))
  "Find the next equinox/solstice on or after DATE."
  (let ((today (calendar-absolute-from-gregorian date)))
    (loop for k    = (floor (1+ (extract-calendar-month date)) 4) then (1+ k)
          for year = (extract-calendar-year date) then (if (> k 3) (1+ year) year)
          for s/e  = (calendar-absolute-from-gregorian (solar-equinoxes/solstices (mod k 4) year))
          when (>= s/e today) return (calendar-gregorian-from-absolute (floor s/e)))))

(defun* days-to-next-equinox/solstice (&optional (date (calendar-current-date)))
  "Return the number of days from DATE to the next equinox/solstice."
  (- (calendar-absolute-from-gregorian (find-next-equinox/solstice date))
     (calendar-absolute-from-gregorian date)))

;;;###autoload
(defun next-equinox/solstice ()
  "Display the date and count of days to the next equinox/solstice."
  (interactive)
  (let ((days (days-to-next-equinox/solstice)))
    (message "%s. %d day%s to go."
             (calendar-date-string (find-next-equinox/solstice))
             days
             (if (= days 1) "" "s"))))

;; equinox.el ends here
