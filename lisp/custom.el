(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command "tex")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "6c9cbcdfd0e373dc30197c5059f79c25c07035ff5d0cc42aa045614d3919dab4" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(debug-on-error nil)
 '(dired-guess-shell-alist-user nil)
 '(display-battery-mode t)
 '(doom-modeline-mode t)
 '(fancy-battery-mode nil)
 '(fci-rule-color "#3C3D37")
 '(flycheck-global-modes nil)
 '(global-eldoc-mode t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(imenu-list-size 0.15)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(leuven-scale-org-agenda-structure t)
 '(magit-diff-use-overlays nil)
 '(nox-python-server "mspyls")
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-files
   '("/mnt/c/Users/lixun/Documents/org/gtd/reading.org" "/mnt/c/Users/lixun/Documents/org/org-roam-files/paper_index.org" "/mnt/c/Users/lixun/Documents/org/journal.org" "/mnt/c/Users/lixun/Documents/org/gtd/learning.org" "/mnt/c/Users/lixun/Documents/org/gtd/webclips.org" "/mnt/c/Users/lixun/Documents/org/gtd/time-tracking.org" "/mnt/c/Users/lixun/Documents/org/gtd/someday.org" "/mnt/c/Users/lixun/Documents/org/gtd/repeater.org" "/mnt/c/Users/lixun/Documents/org/gtd/projects.org" "/mnt/c/Users/lixun/Documents/org/gtd/next.org" "/mnt/c/Users/lixun/Documents/org/gtd/inbox.org" "/mnt/c/Users/lixun/Documents/org/gtd/capture.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-ref-create-notes-hook
   '((lambda nil
       (org-narrow-to-subtree)
       (insert
	(format "cite:%s
"
		(org-entry-get
		 (point)
		 "Custom_ID"))))))
 '(org-ref-note-title-format
   "** TODO %t
 :PROPERTIES:
  :Custom_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :YEAR: %y
  :VOLUME: %v
  :PAGES: %p
  :DOI: %D
  :URL: %U
 :END:

")
 '(package-selected-packages
   '(doom-modeline diff-hl all-the-icons-ivy winum company-tabnine treemacs-magit treemacs-icons-dired treemacs-projectile treemacs markdown-toc grip-mode centaur-tabs eyebrowse org-superstar-mode org-superstar ibuffer-vc ibuffer-projectile all-the-icons-ibuffer all-the-icons-dired diredfl doom-themes helm helm-dash org-roam-server benchmark-init org-latex-impatient org-roam dashboard go-translate vterm switch-window helm-org ebib json-mode hl-todo calibredb ox-hugo ox-latex org-pomodoro which-key nox rainbow-delimiters rainbow-mode all-the-icons fancy-battery projectile Persp-mode spaceline spaceline-config powerline spacemacs-theme yasnippet ox-wk magit request helm-org-rifle poker chess xahk-mode org imenu-list openwith window-numbering ace-jump-mode undo-tree window-number ace-window material-theme org-ref cdlatex auctex company-lsp pyvenv lsp-python-ms use-package flycheck lsp-mode helm-ag expand-region iedit neotree company hungry-delete swiper counsel popwin pyim super-save js2-mode nodejs-repl exec-path-from-shell monokai-theme solarized-theme leuven-theme))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(pyim-page-length 5)
 '(pyim-translate-trigger-char "@")
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(spaceline-helm-mode t)
 '(super-save-auto-save-when-idle t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "#51afef" :foreground "#f2241f" :weight bold))))
 '(diff-hl-change ((t (:foreground "#4078f2" :background nil))))
 '(diff-hl-delete ((t (:background nil))))
 '(diff-hl-insert ((t (:background nil))))
 '(org-latex-and-related ((t (:foreground "#50a14f")))))

(provide 'custom)
