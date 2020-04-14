;;; mac-stuff.el --- Loader for some Mac-specific things -*- lexical-binding: t -*-
;; Copyright 2020 by Dave Pearson <davep@davep.org>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mac-stuff.el loads up some packages of mine that are specific to macOS.

;;; Code:

;; Only bother with this if we're on macOS.
(when is-a-macOS-p

  ;; Tool for getting a Mac's "date of birth".
  (use-package macdob :ensure t)

  ;; Tool for getting some information about a Mac.
  (use-package macinfo :ensure t))

;;; mac-stuff.el ends here
