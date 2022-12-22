(require 'is-a)

(use-package ispell
  :custom
  (ispell-dictionary     "british")
  (ispell-highlight-face 'flyspell-incorrect)
  (ispell-program-name   (if is-a-win32-p "hunspell" "aspell")))

(use-package flyspell
  :diminish
  :commands
  flyspell-mode
  flyspell-prog-mode
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))
