;;; init-repos.el --- Set up package loading and repos we'll load from.  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; init-repos.el takes care of setting up which package repos I use, and
;; brings the package system up. Where the packages themselves live is
;; settled earlier than this, in early-init.el, because startup needs to
;; know before init.el gets a look in.

;;; Code:

(require 'package)

;; Add melpa.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialise the package system.
(package-initialize)

;; Ensure package contents are refreshed on first start.
(unless package-archive-contents
  (package-refresh-contents))

(provide 'init-repos)

;;; init-repos.el ends here
