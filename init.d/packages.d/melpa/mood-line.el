;; -*- lexical-binding: t; -*-

(use-package mood-line
  :ensure t
  :config

  (defface my/mood-line-good-status
    '((t (:weight bold)))
    "Face for good status in mood-line."
    :group 'mood-line)

  (defface my/mood-line-cursor-position
    '((t nil))
    "Face for cursor position in mood-line."
    :group 'mood-line)

  (defface my/mood-line-vc-branch
    '((t nil))
    "Face for VC branch/revision name in mood-line."
    :group 'mood-line)

  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)

  (defun my/mood-line-segment-vc ()
    "Return the VC segment with the branch name propertized with `my/mood-line-vc-branch'."
    (when-let ((vc-text (mood-line-segment-vc)))
      (let ((str (copy-sequence vc-text)))
        (when-let ((space-pos (string-match " " str)))
          (add-text-properties (1+ space-pos) (length str)
                               '(face my/mood-line-vc-branch)
                               str))
        str)))

  (setq mood-line-format
        (mood-line-defformat
         :left
         ((or
           (mood-line-segment-buffer-status)
           (propertize
            (mood-line--get-glyph :buffer-modified)
            'face 'my/mood-line-good-status))
          " "
          (mood-line-segment-buffer-name)
          " : "
          (mood-line-segment-major-mode))
         :right
         ((my/mood-line-segment-vc)
          " "
          (propertize
           (mood-line-segment-cursor-position)
           'face 'my/mood-line-cursor-position))))

  (defun my/mood-line-modus-theme-adjust ()
    "Adjust mood-line faces dynamically using the active Modus Theme palette."
    (modus-themes-with-colors
      ;; Buffer Name (stands out clearly)
      (set-face-attribute 'mood-line-buffer-name nil
                          :foreground cyan
                          :weight 'bold)

      ;; Status Glyph (green when unmodified/good status)
      (set-face-attribute 'my/mood-line-good-status nil
                          :foreground green
                          :weight 'bold)

      ;; Major Mode (subtle grayish-blue, readable but distinct)
      (set-face-attribute 'mood-line-major-mode nil
                          :foreground slate
                          :weight 'bold)

      ;; VC / Version Control Glyph (neutral/clean repo state styled in soft green)
      (set-face-attribute 'mood-line-status-neutral nil
                          :foreground green-faint
                          :weight 'normal)

      ;; VC Branch Name (subtle blue-purple / indigo)
      (set-face-attribute 'my/mood-line-vc-branch nil
                          :foreground indigo
                          :weight 'normal)

      ;; Cursor Position (subtle, warm gold/brown)
      (set-face-attribute 'my/mood-line-cursor-position nil
                          :foreground gold
                          :weight 'normal)))

  (add-hook 'modus-themes-after-load-theme-hook #'my/mood-line-modus-theme-adjust)
  (my/mood-line-modus-theme-adjust)

  (mood-line-mode 1))

;;; mood-line.el ends here
