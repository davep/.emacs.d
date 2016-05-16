(eval-when-compile
  (require 'cl))
(require 'calendar)

;;;###autoload
(defun discordian-date (date)
  "Print DATE from the discordian calendar."
  (interactive (list (calendar-read-date)))
  (message (discordian-date-string date)))

;;;###autoload
(defun* discordian-date-string (&optional (date (calendar-current-date)))
  "Convert DATE to discordian format."
  (let* ((days      ["Sweetmorn" "Boomtime" "Pungenday" "Prickle-Prickle" "Setting Orange"])
         (months    ["Chaos" "Discord" "Confusion" "Bureaucracy" "Aftermath"])
         (day-count [0 31 59 90 120 151 181 212 243 273 304 334])
         (year      (- (extract-calendar-year date) 1900))
         (month     (1- (extract-calendar-month date)))
         (day       (1- (extract-calendar-day date)))
         (julian    (+ (aref day-count month) day)))
    (if (and (= month 1) (= day 28))
        (format "St. Tib's Day, %d" (+ year 3066))
      (format "%s, Day %d of the season of %s, Anno Mung %d"
              (aref days (mod julian 5))
              (1+ (mod julian 73))
              (aref months (floor (/ julian 73)))
              (+ year 3066)))))
