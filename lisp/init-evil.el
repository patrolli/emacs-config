(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-symbol-word-search t))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-delay 0.3)
  (setq-default evil-escape-key-sequence "kj")
  (evil-escape-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   )

(provide 'init-evil)
