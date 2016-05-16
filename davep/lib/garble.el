;;; garble.el --- emacs interface to garble.
;; Copyright 2000,2001 by Dave Pearson <davep@davep.org>
;; $Revision: 1.4 $

;; garble.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; garble.el is an emacs interface for garble, a command line tool for
;; talking to various Garmin <URL:http://www.garmin.com/> GPS units. See
;; <URL:http://freshmeat.net/projects/garble/?highlight=garble> for
;; information about garble.
;;
;; This code aims to provide the primitive functions for talking to a Garmin
;; GPS unit and getting the data back in an Emacs Lisp friendly way. It
;; should be possible to write all sorts of things based around these
;; functions. For example, `sunrise-sunset' could be given a wrapper that
;; ensures you get the information for the location of your GPS unit:
;;
;; (defun sunrise-sunset-here ()
;;   (interactive)
;;   (let* ((pos (garble-get-position))
;;          (calendar-location-name "GPS location")
;;          (calendar-longitude (car pos))
;;          (calendar-latitude (cadr pos)))
;;     (sunrise-sunset)))
;;
;; The latest version of garble.el can be found at:
;;
;;   <URL:http://www.davep.org/emacs/#garble.el>

;;; THANKS:
;;
;; Thanks to Tim Haynes for being too lazy and not finishing his gps.el. One
;; day Tim, one day...

;;; BUGS:
;;
;; o No known bugs.

;;; INSTALLATION:
;;
;; o Drop garble.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add autoloads for the various garble functions to ~/.emacs. At the
;;   very least you want to do something like:
;;
;;   (autoload 'garble-position      "garble" "Display current GPS position" t)
;;   (autoload 'garble-time          "garble" "Display current GPS time" t)
;;   (autoload 'garble-waypoints     "garble" "Display GPS waypoints" t)
;;   (autoload 'garble-trackpoints   "garble" "Display current GPS trackpoints" t)
;;   (autoload 'garble-turn-unit-off "garble" "Power down the GPS unit" t)

;;; Code:

;; Things we need:
(require 'cl)
(require 'parse-time)

;; Attempt to handle older/other emacs.
(eval-and-compile
  
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring)))
  
  ;; If `line-beginning-position' isn't available provide one.
  (unless (fboundp 'line-beginning-position)
    (defun line-beginning-position (&optional n)
      "Return the `point' of the beginning of the current line."
      (save-excursion
        (beginning-of-line n)
        (point))))

  ;; If `line-end-position' isn't available provide one.
  (unless (fboundp 'line-end-position)
    (defun line-end-position (&optional n)
      "Return the `point' of the end of the current line."
      (save-excursion
        (end-of-line n)
        (point)))))

;; Customize options.

(defgroup garble nil
  "Talk to a Garmin GPS unit via garble"
  :group 'hardware
  :group 'external
  :prefix "garble-")

(defcustom garble-program "/usr/local/bin/garble"
  "*Location of the garble program."
  :type  'file
  :group 'garble)

(defcustom garble-device ""
  "*Override garble's default device name."
  :type  'string
  :group 'garble)

(defcustom garble-timeout 1500
  "*Timeout, in milliseconds, to wait for a response from the GPS unit."
  :type  'integer
  :group 'garble)

;; Main code:

(defun garble-call (parameter)
  "Call garble, passing PARAMETER.

If the call is successful the output of garble is returned as a string. If
the call fails NIL is returned."
  (with-temp-buffer
    (let ((args (list garble-program nil (current-buffer) nil parameter "-m" (format "%d" garble-timeout))))
      (unless (zerop (length garble-device))
        (setq args (append args (list "-d" garble-device))))
      (when (zerop (apply #'call-process args))
        (buffer-string)))))

(defun garble-time-string-to-emacs-time (time)
  "Convert the time, as retuned from garble, into emacs' time format."
  (let ((time (parse-time-string time)))
    (setf (tenth time) 0)               ; GMT
    (apply #'encode-time time)))

(defun garble-get-time ()
  "Get the current time from the Garmin unit.

The time is returned in the emacs internal time format.

If the current time can't be acquired NIL is returned."
  (let ((time (garble-call "-z")))
    (when time
      (garble-time-string-to-emacs-time time))))

(defun garble-position-string-to-list (pos)
  "Convert a garble position string into a position list.

The position is returned as a list whose format is:

  (DECIMAL-LONGITUDE DECIMAL-LATITUDE)"
  (car (read-from-string (concat "(" (remove ?, pos) ")"))))

(defun garble-get-position ()
  "Get the current position from the Garmin unit.

The position is returned as a list whose format is:

  (DECIMAL-LONGITUDE DECIMAL-LATITUDE)

If the position can't be acquired NIL is returned."
  (let ((pos (garble-call "-p")))
    (when pos
      (garble-position-string-to-list pos))))

(defun garble-stringlist-to-list (list convert)
  "Convert a garble list into a lisp list.

LIST is a string of data to be converted, each line is an element in the
list.

CONVERT is the function to use to convert each line."
  (when list
    (with-temp-buffer
      (setf (buffer-string) list)
      (setf (point) (point-min))
      (loop until (eobp)
            for item = (funcall convert
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
            when item collect item
            do (forward-line)))))
    
(defun garble-get-waypoints ()
  "Get the waypoint list from the Garmin unit.

The return list is a list of waypoint items, each waypoint item is a list
whose format is:

  (NAME POSITION COMMENT)

POSITION is a list whose format is:

  (DECIMAL-LONGITUDE DECIMAL-LATITUDE)

If the waypoint list can't be acquired NIL is returned."
  (garble-stringlist-to-list
   (garble-call "-w")
   #'(lambda (wp)
       (when (posix-string-match "^\\(.*\\) / \\(.*\\) / \\(.*\\)$" wp)
         (list (match-string 1 wp)
               (garble-position-string-to-list (match-string 2 wp))
               (match-string 3 wp))))))

(defun garble-get-tracks ()
  "Get the current track data from the Garmin unit.

The return list is a list of trackpoints, each trackpoint item is a list
whose format is:

  (POSITION TIME)

POSITION is a list whose format is:

  (DECIMAL-LONGITUDE DECIMAL-LATITUDE)

TIME is in emacs' internal time format.

If the track data can't be acquired NIL is returned."
  (garble-stringlist-to-list
   (garble-call "-t")
   #'(lambda (tp)
       (when (posix-string-match "^\\(.*\\) / \\(.*\\)$" tp)
         (list (garble-position-string-to-list (match-string 1 tp))
               (garble-time-string-to-emacs-time (match-string 2 tp)))))))

;;;###autoload
(defun garble-position ()
  "Interactively query the position from the Garmin GPS unit."
  (interactive)
  (let ((pos (garble-get-position)))
    (if pos
        (message "Latitude %f, longitude %f" (car pos) (cadr pos))
      (error "Can't get position information"))))

;;;###autoload
(defun garble-time ()
  "Interactively query the time from the Garmin GPS unit."
  (interactive)
  (let ((time (garble-get-time)))
    (if time
        (message "%s" (current-time-string time))
      (error "Can't get time information"))))

;;;###autoload
(defun garble-waypoints ()
  "List the waypoints stored in the Garmin GPS unit."
  (interactive)
  (let ((waypoints (garble-get-waypoints)))
    (if waypoints
        (with-output-to-temp-buffer "*Garble Waypoints*"
          (princ "Name        Longitude  Latitude   Comment\n")
          (princ "==========  =========  =========  ==============================\n")
          (loop for wp in waypoints
                do (princ (format "%-10s  %9.4f  %9.4f  %s\n"
                                  (nth 0 wp)
                                  (car (nth 1 wp))
                                  (cadr (nth 1 wp))
                                  (nth 2 wp)))))
      (error "Can't get waypoints"))))

;;;###autoload
(defun garble-trackpoints ()
  "List the trackpoints stored in the Garmin GPS unit."
  (interactive)
  (let ((trackpoints (garble-get-tracks)))
    (if trackpoints
      (with-output-to-temp-buffer "*Garble Trackpoints*"
        (princ "Longitude  Latitude   Time\n")
        (princ "=========  =========  ==============================\n")
        (loop for tp in trackpoints
              do (princ (format "%9.4f  %9.4f  %s\n"
                                (car (nth 0 tp))
                                (cadr (nth 0 tp))
                                (current-time-string (nth 1 tp))))))
      (error "Can't get trackpoints"))))

;;;###autoload
(defun garble-turn-unit-off ()
  "Turn off the Garmin unit."
  (interactive)
  (garble-call "-o"))

(provide 'garble)

;;; garble.el ends here.
