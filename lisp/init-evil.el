(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   )

(provide 'init-evil)
