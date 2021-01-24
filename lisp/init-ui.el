(tool-bar-mode -1)

(scroll-bar-mode -1)
(global-linum-mode -1)
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
(use-package leuven-theme :ensure :defer)
(use-package monokai-theme :ensure :defer)
(use-package spacemacs-theme :ensure :defer)
;; (load-theme 'leuven t)
;; (use-package circadian
;;   :ensure t
;;   :config
;;   ;; chengdu 
;;   (setq calendar-latitude 30.67)
;;   (setq calendar-longitude 104.06)
;;   (setq circadian-themes '(("8:00" . tsdh-light)
;;                            ;; ("12:00" . material-light)
;; 			  ;; ("18:00" . spacemacs-dark)
;; 			   ))
;;   ;; according to sunrise&sunset
;;   ;; (setq circadian-themes '((:sunrise . apropospriate-light)
;;   ;;                          (:sunset  . nord)))
;;   (circadian-setup))
;; (load-theme 'tsdh-light t)

(use-package doom-themes
       :ensure t
       :config
       (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
         doom-themes-enable-italic t) ; if nil, italics is universally disabled
       (load-theme 'doom-one-light t)
       (doom-themes-org-config)

     ;; (setq org-src-fontify-natively nil)

     (with-eval-after-load 'org 
       (custom-theme-set-faces
        'user
        `(org-latex-and-related ((t (:foreground ,(doom-color 'green))))))
       (setq org-highlight-latex-and-related '(latex script entities))))




;; (setq leuven-scale-outline-headlines nil)
;; (setq leuven-scale-org-agenda-structure nil)
;;(load-theme 'material-light t)

;; (use-package powerline :ensure)
;; (require 'powerline)
;; (powerline-default-theme)

(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (use-package persp-mode :ensure)
  (use-package projectile :ensure)
  (require 'spaceline-config)
  (require 'spaceline-segments)
  (spaceline-helm-mode 1)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-persp-name-on)
  (spaceline-toggle-projectile-root-on))

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :config
  :functions (all-the-icons-icon-for-buffer))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5))))

(provide 'init-ui)
