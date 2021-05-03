(defun lxs/startup-window ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode -1)
  (menu-bar-mode -1) ;; 默认不开启 menu-bar, 只在需要时手动打开
  (setq initial-frame-alist (quote ((fullscreen . fullboth))))
  (global-hl-line-mode 1)
  )

(scroll-bar-mode -1)
(global-linum-mode -1)
(setq initial-frame-alist (quote ((fullscreen . fullboth))))
(global-hl-line-mode 1)
;; (setq inhibit-splash-screen t)
;; open emacs default full-screen

(use-package doom-themes
       :ensure t
       :config
       (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
         doom-themes-enable-italic t) ; if nil, italics is universally disabled
       ;; (load-theme 'doom-one t)
       ;; (load-theme 'doom-gruvbox-light)
       (load-theme 'modus-vivendi)
       (doom-themes-org-config)
       ;; (doom-themes-neotree-config)

     (setq org-src-fontify-natively t)

     (with-eval-after-load 'org 
       (custom-theme-set-faces
        'user
        `(org-latex-and-related ((t (:foreground ,(doom-color 'green))))))
       (setq org-highlight-latex-and-related '(latex script entities)))
     )


;; (setq leuven-scale-outline-headlines nil)
;; (setq leuven-scale-org-agenda-structure nil)
;;(load-theme 'material-light t)

;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (setq powerline-default-separator 'slant)
;;   :config
;;   (use-package persp-mode :ensure)
;;   (use-package projectile :ensure)
;;   (require 'spaceline-config)
;;   (require 'spaceline-segments)
;;   (spaceline-helm-mode 1)
;;   (spaceline-spacemacs-theme)
;;   (spaceline-toggle-minor-modes-off)
;;   (spaceline-toggle-buffer-size-off)
;;   (spaceline-toggle-persp-name-on)
;;   (spaceline-toggle-projectile-root-on))

;; use doom-modeline instead of spaceline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-modification-icon nil))

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :init
  :config
  :functions (all-the-icons-icon-for-buffer))


(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-ibuffer))



;; automatically change the theme without be mess up
(defcustom load-theme-before-hook nil
  "Functions to run before load theme."
  :type 'hook)

(defcustom load-theme-after-hook nil
  "Functions to run after load theme."
  :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
  "A wrapper of hooks around `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'load-theme-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

(provide 'init-ui)
