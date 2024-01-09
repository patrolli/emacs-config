(use-package ef-themes
  :ensure t
  :config
  (my-ensure 'counsel)
  (counsel-load-theme-action "ef-spring"))

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

(use-package nerd-icons
  :ensure t)

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

;; Specify font type of code block in markdown/org modes, when using modus theme
(set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 1.0)

(provide 'init-ui)
