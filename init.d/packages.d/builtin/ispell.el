(require 'is-a)

(use-package ispell
  :custom
  (ispell-dictionary     "british")
  (ispell-highlight-face 'flyspell-incorrect)
  :config
  ;; Try and get aspell working on a Windows machine.
  (let ((aspell "C:/Program Files (x86)/Aspell/bin/"))
    (when (and is-a-win32-p (file-exists-p aspell))
      (push aspell exec-path)
      (setq ispell-program-name "aspell"))))

(use-package flyspell
  :diminish
  :commands
  flyspell-mode
  flyspell-prog-mode
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))
