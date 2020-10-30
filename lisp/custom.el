(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(dired-guess-shell-alist-user nil)
 '(flycheck-global-modes nil)
 '(imenu-list-position (quote left))
 '(imenu-list-size 0.15)
 '(leuven-scale-org-agenda-structure t)
 '(org-agenda-files
   (quote
    ("/mnt/c/Users/lixun/Documents/org/tdd.org" "/mnt/c/Users/lixun/Documents/org/paper-taxonomy.org" "/mnt/c/Users/lixun/Documents/org/scratch.org" "/mnt/c/Users/lixun/Documents/org/paper-reading.org" "/mnt/c/Users/lixun/Documents/org/inbox.org" "/mnt/c/Users/lixun/Documents/org/journal.org" "/mnt/c/Users/lixun/Documents/org/learning-notes.org" "/mnt/c/Users/lixun/Documents/org/notes.org")))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-modules
   (quote
    (ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m)))
 '(org-ref-create-notes-hook
   (quote
    ((lambda nil
       (org-narrow-to-subtree)
       (insert
	(format "cite:%s
"
		(org-entry-get
		 (point)
		 "Custom_ID")))))))
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
   (quote
    (yasnippet ox-wk magit leetcode request helm-org-rifle poker chess xahk-mode org imenu-list openwith window-numbering ace-jump-mode undo-tree window-number ace-window material-theme org-ref cdlatex auctex company-lsp pyvenv lsp-python-ms use-package flycheck lsp-mode helm-ag expand-region iedit neotree company hungry-delete smex swiper counsel smartparens popwin pyim super-save cnfonts js2-mode nodejs-repl exec-path-from-shell monokai-theme solarized-theme leuven-theme)))
 '(pyim-dicts
   (quote
    ((:name "pyim-bigdict" :file "/mnt/c/Users/lixun/Downloads/pyim-bigdict.pyim"))))
 '(pyim-page-length 7)
 '(super-save-auto-save-when-idle t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
