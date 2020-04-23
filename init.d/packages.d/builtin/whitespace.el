(use-package whitespace
  :custom
  (whitespace-style '(face trailing empty tabs tab-mark))
  :config
  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     (whitespace-mode (if (buffer-file-name) 1 -1)))))
