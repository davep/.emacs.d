(use-package obsidian
  :ensure t
  :config
  (global-obsidian-mode t)
  (obsidian-backlinks-mode t)
  :custom
  (obsidian-directory "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Kennis")
  (obsidian-inbox-directory "Inbox")
  (markdown-enable-wiki-links t)
  :bind
  ;; Create note
  ("<f12> o n" . obsidian-capture)
  ;; If you prefer you can use `obsidian-insert-wikilink'
  ("<f12> o l" . obsidian-insert-link)
  ;; Open file pointed to by link at point
  ("<f12> o g" . obsidian-follow-link-at-point)
  ;; Open a different note from vault
  ("<f12> o o" . obsidian-jump)
  ;; Follow a backlink for the current file
  ("<f12> o b" . obsidian-backlink-jump))
