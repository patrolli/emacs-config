(use-package go-translate
  :ensure nil
  :bind
  ("C-c t" . go-translate)
  :config
  (require 'facemenu)
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-buffer-follow-p t)
  )

;; Persistent the scratch buffer
;; FIXME: scrach buffer 打开总是 org-agenda mode
(use-package persistent-scratch
  :diminish
  :custom
  (persistent-scratch-backup-directory (concat user-emacs-directory ".persistent-scratch-backup"))
  :bind (:map persistent-scratch-mode-map
         ([remap kill-buffer] . (lambda (&rest _)
                                  (interactive)
                                  (user-error "Scrach buffer cannot be killed")))
         ([remap revert-buffer] . persistent-scratch-restore)
         ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode)))

(provide 'init-utils)
