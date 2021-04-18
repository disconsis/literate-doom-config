;; restore substitute behaviour with s/S
(package! evil-snipe :disable t)

;; C-[ works well enough for me
(package! evil-escape :disable t)

;; syntax-highlighted diffs
(package! magit-delta)

;; more interactive org-roam graphs
(package! org-roam-server)

;; presentations from org
;;(package! emacs-reveal)



;; ------------
;;    themes
;; ------------

(package! tao-theme)

;; get all updates for org-roam
(package! org-roam :pin nil)

;; toggle latex fragments on the go
(package! org-fragtog)

;; org in non-org files
(package! outshine)

;; utility package
(package! move-text)

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

(package! base16-theme)

;; minimal themes
(package! brutalist-theme)
(package! minimal-theme)

;; random themes
(package! colonoscopy-theme)

;; a collection of good themes
(package! kaolin-themes)

;; poet, for org-ricing
(package! poet-theme)


;; research
(package! z3-mode
  :recipe (:host github :repo "disconsis/z3-mode"))

;; django
(package! pony-mode)

;; strace
(package! strace-mode)

(package! csv-mode)

;; org-mode
(package! ob-session-async
  :recipe (:host github :repo "jackkamm/ob-session-async"))

;; sml
(package! sml-mode)

;; ada
(package! ada-mode) ;; some issues with dependencies

;; epub
(package! nov)

;; -----
;;  PDF
;; -----
;; FIXME
;; (package! pdf-continuous-scroll-mode
;;   :recipe (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

;; general folding (esp. elixir)
(package! yafolding)

;; take code screenshots!
(package! escr
  :recipe (:host github :repo "atykhonov/escr"))
