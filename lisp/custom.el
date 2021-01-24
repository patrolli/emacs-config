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
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(dired-guess-shell-alist-user nil)
 '(fci-rule-color "#3C3D37")
 '(flycheck-global-modes nil)
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
 '(leuven-scale-org-agenda-structure t)
 '(magit-diff-use-overlays nil)
 '(nox-python-server "mspyls")
 '(org-agenda-files
   '("/mnt/c/Users/lixun/Documents/org/gtd/learning.org" "/mnt/c/Users/lixun/Documents/org/gtd/webclips.org" "/mnt/c/Users/lixun/Documents/org/gtd/time-tracking.org" "/mnt/c/Users/lixun/Documents/org/gtd/someday.org" "/mnt/c/Users/lixun/Documents/org/gtd/repeater.org" "/mnt/c/Users/lixun/Documents/org/gtd/projects.org" "/mnt/c/Users/lixun/Documents/org/gtd/next.org" "/mnt/c/Users/lixun/Documents/org/gtd/inbox.org" "/mnt/c/Users/lixun/Documents/org/gtd/capture.org"))
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
   '(doom-themes helm helm-dash org-roam-server benchmark-init org-latex-impatient org-roam dashboard go-translate vterm switch-window helm-org ebib json-mode hl-todo calibredb ox-hugo ox-latex org-pomodoro which-key nox rainbow-delimiters rainbow-mode all-the-icons fancy-battery projectile persp-mode Persp-mode spaceline spaceline-config powerline spacemacs-theme yasnippet ox-wk magit leetcode request helm-org-rifle poker chess xahk-mode org imenu-list openwith window-numbering ace-jump-mode undo-tree window-number ace-window material-theme org-ref cdlatex auctex company-lsp pyvenv lsp-python-ms use-package flycheck lsp-mode helm-ag expand-region iedit neotree company hungry-delete smex swiper counsel smartparens popwin pyim super-save js2-mode nodejs-repl exec-path-from-shell monokai-theme solarized-theme leuven-theme))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(pyim-dicts
   '((:name "pyim-bigdict" :file "/mnt/c/Users/lixun/Downloads/pyim-bigdict.pyim")))
 '(pyim-page-length 7)
 '(pyim-translate-trigger-char "v")
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
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
