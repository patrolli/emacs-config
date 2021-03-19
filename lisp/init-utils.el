(use-package go-translate
  :ensure nil
  :bind
  ("C-c t" . go-translate)
  :config
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-buffer-follow-p t)
  )

(provide 'init-utils)
