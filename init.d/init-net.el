;;; init-net.el --- Configuration that affects Net-base comms.

;;; Commentary:
;;
;; init-net.el configures things like Email sending and the url package.
;; Note that any package-based config will always be done via `use-package'
;; in the relevant init-package-* file, as a preference. This file is only
;; used for those changes that don't directly relate to a package.

;;; Code:

(setq
 ;; Who I am.
 user-mail-address "davep@davep.org"
 ;; Bounce my emails off gmail.
 smtpmail-smtp-server  "smtp.gmail.com"
 smtpmail-smtp-service 587
 send-mail-function    #'smtpmail-send-it
 ;; Configure the URL package.
 url-configuration-directory "~/.emacs.d/.url/"
 url-cache-directory         "~/.emacs.d/.url/cache"
 url-cookie-file             "~/.emacs.d/.url/cookies"
 ;; Security.
 nsm-save-host-names t
 nsm-settings-file   "~/.emacs.d/.network-security.data"
 oauth2-token-file   "~/.emacs.d/.oauth2.plstore")

(provide 'init-net)

;;; init-editing.el ends here
