(use-package elec-pair
  :commands electric-pair-mode
  :init
  (electric-pair-mode t)
  :custom
  (electric-pair-inhibit-predicate
   (lambda (c)
     (or
      ;; This isn't ideal. I often don't want pair completion inside the
      ;; minibuffer, but there are times when I might (`eval-expression'
      ;; being one such time when I'd want it). Given the former is more
      ;; common than the latter, and I can't really see a good way of only
      ;; turning this off for some things, let's turn it off any time we're
      ;; in the minibuffer.
      (eq major-mode 'minibuffer-inactive-mode)
      ;; Don't complete { when in web-mode and using the Django engine, as
      ;; it does its own thing.
      (and
       (eq major-mode 'web-mode)
       (string= (symbol-value 'web-mode-engine) "django")
       (= c ?{))))))
