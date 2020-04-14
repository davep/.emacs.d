;;; commoji.el --- commoji.el package loader -*- lexical-binding: t -*-
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
;; commoji.el is used to insert relevant emoji into Git commit messages,
;; which is mostly cute (and useful) when working with GitHub and GitLab as
;; your git forge.

;;; Code:

(use-package commoji
  :ensure t
  :bind
  ("<f12> g e" . commoji))

;;; commoji.el ends here
