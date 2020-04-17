(require 'init-local)

(use-package calendar
  :config
  (setq
   diary-file             (local-emacs-directory "diary")
   calendar-latitude      55.9
   calendar-longitude     -3.2
   calendar-location-name "Edinburgh, Scotland"))
