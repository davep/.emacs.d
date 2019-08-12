;;; init-net.el --- Configuration that affects Net-base comms.

;;; Commentary:
;;
;; init-net.el configures things like Email sending and the url package.
;; Note that any package-based config will always be done via `use-package'
;; in the "relevant packages/init-package-*.el" file, as a preference. This
;; file is only used for those changes that don't directly relate to a
;; package.

;;; Code:

(require 'nsm)
(require 'url)
(require 'smtpmail)

(setq
 ;; Who I am.
 user-mail-address "davep@davep.org"
 ;; Bounce my emails off gmail.
 smtpmail-smtp-server  "smtp.gmail.com"
 smtpmail-smtp-service 587
 send-mail-function    #'smtpmail-send-it
 ;; Configure the URL package.
 url-configuration-directory (local-emacs-directory "url/")
 url-cache-directory         (local-emacs-directory "url/cache")
 url-cookie-file             (local-emacs-directory "url/cookies")
 ;; Security.
 nsm-save-host-names t
 nsm-settings-file   (local-emacs-directory "network-security.data")

(provide 'init-net)

;;; init-net.el ends here
