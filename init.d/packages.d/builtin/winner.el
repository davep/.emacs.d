;; -*- lexical-binding: t; -*-

(use-package winner
  :init
  (winner-mode 1)
  :bind
  ("s-[" . winner-undo)
  ("s-]" . winner-redo))

;;; winner.el ends here
