(when is-a-unix-p
  (use-package emojify
    :if is-a-unix-window-p
    :commands global-emojify-mode
    :config
    (setq emojify-emojis-dir (local-emacs-directory "emojis"))
    (add-to-list 'emojify-inhibit-major-modes 'restclient-mode)
    (add-to-list 'emojify-inhibit-major-modes 'prog-mode)
    (add-to-list 'emojify-inhibit-major-modes 'conf-mode)
    :init
    (global-emojify-mode)
    :ensure t))
