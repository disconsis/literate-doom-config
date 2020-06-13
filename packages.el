;; restore substitute behaviour with s/S
(package! evil-snipe :disable t)

;; C-[ works well enough for me
(package! evil-escape :disable t)

;; syntax-highlighted diffs
(package! magit-delta)

;; more interactive org-roam graphs
(package! org-roam-server)

;; themes
(package! tao-theme)

;; get all updates for org-roam
(package! org-roam :pin nil)
