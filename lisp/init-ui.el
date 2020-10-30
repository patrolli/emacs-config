(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq inhibit-splash-screen t)
;; open emacs default full-screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; highlight current line
(global-hl-line-mode 1)
;; monokai
;;(add-to-list 'my/packages 'monokai-theme)
;;(load-theme 'monokai 1)
;; solarized-light
;;(add-to-list 'my/packages 'solarized-theme)
;;(load-theme 'solarized-dark t)
;; leuven theme
;;(add-to-list 'my/packages 'leuven-theme)
(load-theme 'leuven t)
(setq leuven-scale-outline-headlines nil)
(setq leuven-scale-org-agenda-structure nil)
;;(load-theme 'material-light t)
(provide 'init-ui)
