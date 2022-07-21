;; projectile configuration
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c C-x p" . projectile-command-map))
  :config
  (setq projectile-switch-project-action #'projectile-switch-to-buffer)
  )

(provide 'init-projectile)
