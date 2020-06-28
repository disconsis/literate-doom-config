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

;; toggle latex fragments on the go
(package! org-fragtog)

;; utility package
(package! move-text)


;; ------------
;;    themes
;; ------------

;; packs a number of themes: https://github.com/owainlewis/emacs-color-themes
;; - granger
;; - junio
;; - mccarthy
;; - fogus
(package! sublime-themes)

;; famed for great org-specific faces
(package! leuven-theme)

(package! ir-black-theme)
(package! clues-theme)

;; highlights by changing bg color of some faces
(package! soothe-theme)

;; fun grey-ish theme, great light theme => sorta like zenburn
(package! apropospriate-theme)

;; pair of high-contrast themes
(package! modus-operandi-theme)
(package! modus-vivendi-theme)

;; the fabled zenburn
(package! zenburn-theme)

(package! afternoon-theme)

;; some (good?) light themes
(package! ample-theme)        ;; Great dark theme, dull yellow light theme
(package! anti-zenburn-theme) ;; grey, but less
(package! plan9-theme)        ;; it's hiiiiigh noon
(package! flatui-theme)       ;; high-constrast, slightly bluish
