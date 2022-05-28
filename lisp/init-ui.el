(scroll-bar-mode -1)
(global-linum-mode -1)
(global-hl-line-mode 1)

;; randomly pick my favourite themes
;; ref: https://github.com/redguardtoo/emacs.d/blob/5c0f5702eebc430363d05e058077f65c4d6c2a2d/lisp/init-theme.el#L10
(defvar my-favourite-color-themes '(doom-gruvbox doom-one-light doom-zenburn
						 modus-operandi doom-dracula
						 doom-material-dark doom-manegarm
						 doom-peacock)
  "A set of themes randomly picked when eamcs starts up")

(defun random-choice (items)
  (let* ((size (length items))
	 (index (random size)))
    (nth index items)))

(defun my-pick-random-theme (themes)
  (my-ensure 'counsel)
  (let* ((available-themes (mapcar 'symbol-name themes))
	 (theme (nth (random (length available-themes)) available-themes)))
    (counsel-load-theme-action theme)
    (message "Color theme [%s] loaded" theme)))

(defun lxs/pick-my-favorite-themes ()
  (interactive)
  (my-pick-random-theme (or my-favourite-color-themes
			    (custom-available-themes))))

(use-package doom-themes
       :ensure t
       :config
       (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
         doom-themes-enable-italic t) ; if nil, italics is universally disabled
       ;; (load-theme 'doom-one t)
       ;; (load-theme 'doom-gruvbox-light)
       ;; (load-theme 'modus-vivendi)
       ;; (load-theme 'doom-zenburn t)
       (call-interactively 'lxs/pick-my-favorite-themes)
       (doom-themes-org-config)
       ;; (doom-themes-neotree-config)
     (setq org-src-fontify-natively t)
     (with-eval-after-load 'org 
       (custom-theme-set-faces
        'user
        `(org-latex-and-related ((t (:foreground ,(doom-color 'green))))))
       (setq org-highlight-latex-and-related '(latex script entities))))

;; use doom-modeline instead of spaceline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-modification-icon nil))


(setq mode-line-misc-info (delete (assoc 'which-function-mode
					 mode-line-misc-info) mode-line-misc-info))

(add-hook 'prog-mode-hook '(lambda () (setq header-line-format '(which-func-mode ("" which-func-format " ")))))

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :init
  :config
  :functions (all-the-icons-icon-for-buffer))

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
