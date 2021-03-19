;; projectile configuration
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c C-x p" . projectile-command-map))
  :config
  )

(provide 'init-projectile)
