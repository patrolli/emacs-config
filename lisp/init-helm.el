(use-package helm
  :ensure t
  :config
  (setq helm-mini-default-sources
	'(helm-source-bookmarks helm-source-recentf helm-source-buffers-list helm-source-buffer-not-found helm-source-projectile-files-list))
  ; 单独的 frame 显示 helm 搜索
  (setq helm-display-function 'helm-default-display-buffer))

(use-package helm-projectile
  :ensure t)

(provide 'init-helm)
