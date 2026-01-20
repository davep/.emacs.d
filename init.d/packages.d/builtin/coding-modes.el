(use-package cc-mode
  :custom
  (c-basic-offset 4)
  (c-offsets-alist '((inline-open . 0)
                     (case-label . +)))
  (c-default-style '((c-mode    . "BSD")
                     (c++-mode  . "BSD")
                     (java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "gnu")))
  :bind
  (:map c-mode-map   ("RET" . newline-and-indent))
  (:map c++-mode-map ("RET" . newline-and-indent)))

(use-package js2-mode
  :mode "\\.js$"
  :custom
  (js-switch-indent-offset 4))

(use-package typescript-ts-mode
  :mode "\\.ts$"
  :custom
  (typescript-ts-mode-indent-offset 4))

(use-package tsx-ts-mode
  :mode "\\.tsx$"
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package go-mode
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (setq-local indent-tabs-mode nil)))

(use-package python
  :custom
  (python-indent-guess-indent-offset nil)
  (python-shell-interpreter "python3")
  (python-fill-docstring-style 'pep-257-nn))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Make it clear where the empty lines are
            (setq-local indicate-empty-lines t)
            ;; Set up for auto-filling comments.
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode 1)))

(add-to-list 'auto-mode-alist '("\\.tcss$" . scss-mode))

;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
